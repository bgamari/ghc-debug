#include <cerrno>
#include <cstdint>
#include <cstdbool>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <arpa/inet.h>


#include <thread>
#include <functional>

#include <Rts.h>
#include "socket.h"
#include "trace.h"
#include "parser.h"
#include <stdarg.h>
#include <stdio.h>

#if !defined(THREADED_RTS)
#error You must use a patched version of cabal-install which includes - https://github.com/haskell/cabal/pull/7183
#endif

// This used to be 4096 but that was too small
#define MAX_CMD_SIZE 10000

#define WORD_SIZE sizeof(unsigned long)
#define INFO_TABLE_SIZE sizeof(StgInfoTable)

/*
 * Wire format:
 *
 * Request from debugger consists of
 *   - uint32_t frame length
 *   - uint16_t command
 *   - payload
 *
 * Response consists of one of more frames of the form
 *   - uint32_t frame length
 *   - uint16_t response_code
 *   - payload
 *
 * Payload may be split across multiple frames
 *
 */

enum commands {
    CMD_VERSION = 1,
    CMD_PAUSE   = 2,
    CMD_RESUME  = 3,
    CMD_GET_ROOTS = 4,
    CMD_GET_CLOSURES = 5,
    CMD_GET_INFO_TABLES = 6,
    CMD_GET_BITMAP = 7,
    CMD_POLL = 8,
    CMD_SAVED_OBJECTS = 9,
    CMD_FIND_PTR = 10,
    CMD_CON_DESCR = 11,
    CMD_SOURCE_INFO = 12,
    CMD_GET_STACK = 13,
    CMD_BLOCKS = 14,
    CMD_BLOCK = 15
};

enum response_code {
    RESP_OKAY = 0,
    RESP_OKAY_CONTINUES = 1,
    // Error responses
    RESP_BAD_COMMAND = 0x100,
    RESP_ALREADY_PAUSED = 0x101,
    RESP_NOT_PAUSED = 0x102,
    RESP_NO_RESUME = 0x103,
};

// RTS signatures
// FIXME: These need to be made not private in GHC.
extern "C" {
//typedef void (*FindPtrCb)(void *user, StgClosure *);
//void findPtrCb(FindPtrCb , void* , P_);
//void findPtr(P_, int);
}

extern "C" Capability **capabilities;

const int maxSavedObjects = 20;

static struct savedObjectsState {
    StgWord n_objects;
    StgStablePtr objects[maxSavedObjects];
} g_savedObjectState;

class Response {
  private:
    Socket &sock;
    size_t buf_size;
    char *const buf;
    char *tail;

    struct Header {
        uint32_t len;
        uint16_t status; // 0 == success
    };

    void flush(response_code status) {
        if (status != RESP_OKAY_CONTINUES || this->tail != this->buf + sizeof(Header)) {
            size_t len = this->tail - this->buf;
            trace("LEN: %lu", len);
            uint32_t len_payload;
            uint16_t status_payload;
            len_payload=htonl(len);
            status_payload = htons(status);
            trace("STATUS: %d\n", status);
            // Header is the length
            this->sock.write((char *) &len_payload, sizeof(uint32_t));
            // Then status
            this->sock.write((char *) &status_payload, sizeof(uint16_t));
            // then the body, usually empty
    trace("FLUSHING(%lu)( ", len);
    for (int i = 0; i < len; i++)
    {
      trace("%02X", buf[i]);
    }
    trace("\n");
            this->sock.write(this->buf, len);
            this->tail = this->buf;
        }
    }

  public:
    Response(Socket &sock) : Response(sock, 1024) { }

    Response(Socket &sock, size_t buf_size)
      : sock(sock),
        buf_size(buf_size),
        buf(new char[buf_size]),
        tail(buf)
        { }

    ~Response() {
        delete[] this->buf;
    }

    template<typename T>
    void write(T x) {
        write((const char *) &x, sizeof(T));
    }

    void write(const char *buf, size_t len) {
        if (len > this->buf_size) {
            trace("LEN TOO BIG %d, %d\n", len, this->buf_size);
            this->flush(RESP_OKAY_CONTINUES);
            uint32_t len_payload;
            uint16_t status_payload;
            len_payload=htonl(len);
            status_payload = htons(RESP_OKAY_CONTINUES);
            // Header is the length
            this->sock.write((char *) &len_payload, sizeof(uint32_t));
            // Then status
            this->sock.write((char *) &status_payload, sizeof(uint16_t));
            trace("WROTE HEADER\n");
            this->sock.write(buf, len);
            trace("WRITING BIG CLOSURE\n");
        } else {
            if (this->tail + len >= this->buf + this->buf_size) {
                trace("FLUSHING: ");
                this->flush(RESP_OKAY_CONTINUES);
            }

            //trace("ADDING(%lu)( ", len);
            //for (int i = 0; i < len; ++i) std::cout << std::hex << (int) buf[i] << ' ';
            //std::cout << std::dec << std::endl ;
            memcpy(this->tail, buf, len);
            this->tail += len;
        }
    }

    void finish(enum response_code status) {
        trace("FINISH: %d\n", status);
        this->flush(status);
    }

    Response(Response &x) = delete;
};

static bool paused = false;
static PauseToken * r_paused;
static Response * r_poll_pause_resp = NULL;

static StgStablePtr rts_saved_closure = NULL;

extern "C"
void pause_mutator() {
  r_paused = rts_pause();
  if (r_poll_pause_resp != NULL){
      r_poll_pause_resp->finish(RESP_OKAY);
  }
  paused = true;
}

extern "C"
void resume_mutator() {
  //trace("Resuming %p %p\n", r_paused.pausing_task, r_paused.capabilities);
  rts_resume(r_paused);
  paused = false;
}


void collect_threads(std::function<void(StgTSO*)> f) {
    for (int g=0; g < RtsFlags.GcFlags.generations; g++) {
        StgTSO *tso = generations[g].threads;
        while (tso != END_TSO_QUEUE) {
            f(tso);
            tso = tso->global_link;
        }
    }
}

// helper evac_fn
void evac_fn_helper(void *user, StgClosure **root) {
    std::function<void(StgClosure*)> *f = static_cast<std::function<void(StgClosure*)>*>(user);
    (*f)(*root);
}

void collect_threads_callback(void *user, StgTSO * tso){
  ((Response *) user)->write((uint64_t) tso);
}

void collect_misc_callback(void *user, StgClosure * clos){
  ((Response *) user)->write((uint64_t) clos);
}

void inform_callback(void *user, PauseToken * p){
  ((Response *) user)->finish(RESP_OKAY);
  r_paused = p;
  //trace("Informed %p %p\n", r_paused.pausing_task, r_paused.capabilities);
  paused = true;
}

static void write_large_bitmap(Response& resp, StgLargeBitmap *large_bitmap, StgWord size) {
    uint32_t b = 0;

    for (uint32_t i = 0; i < size; b++) {
        StgWord bitmap = large_bitmap->bitmap[b];
        uint32_t j = stg_min(size-i, BITS_IN(W_));
        i += j;
        for (; j > 0; j--) {
            resp.write((uint8_t) !(bitmap & 1));
            bitmap = bitmap >> 1;
        }
    }
}

static void write_string(Response& resp, const char * s){
    uint32_t len_payload;
    len_payload=htonl(strlen(s));
    resp.write(len_payload);
    trace("SIZE: %d", strlen(s));
    uint32_t i;
    trace("WRITING: %s\n", s);
    for (i = 0; i < strlen(s); i++){
      resp.write(s[i]);
    }
}


static void write_block(Response& resp, bdescr * bd){
  resp.write(bd);
  uint32_t len_payload = htonl(BLOCK_SIZE);
  resp.write(len_payload);
  resp.write((const char *) bd, BLOCK_SIZE);
}

static void write_blocks(Response& resp, bdescr * bd){
    for (; bd != NULL; bd = bd->link){
      write_block(resp, bd);
    }
}

/* return non-zero on error */
static int handle_command(Socket& sock, const char *buf, uint32_t cmd_len) {
    printf("GENERATIONS %lu", generations[1].n_blocks);
    trace("HANDLE: %d\n", cmd_len);
    Parser p(buf, cmd_len);
    Response resp(sock);
    trace("P %lu\n", p.available());
    uint32_t cmd = ntohl(p.get<uint32_t>());
    trace("CMD: %d\n", cmd);
    switch (cmd) {
      case CMD_VERSION:
        uint32_t ver_payload;
        ver_payload=htonl(0);
        resp.write(ver_payload);
        resp.finish(RESP_OKAY);
        break;

      case CMD_PAUSE:
        trace("PAUSE: %d", paused);
        if (paused) {
            trace("ALREADY");
            resp.finish(RESP_ALREADY_PAUSED);
        } else {
            pause_mutator();
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_RESUME:
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else if (r_poll_pause_resp){
            // See #7, resuming after the Haskell process pauses
            // is a direct train to a segfault and I can't work out how to fix
            // it. Therefore it's just disallowed for now.
            resp.finish(RESP_NO_RESUME);
        } else {
            resume_mutator();
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_GET_ROOTS:
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else {
            rts_listThreads(&collect_threads_callback, &resp);
            rts_listMiscRoots(&collect_misc_callback, &resp);
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_GET_CLOSURES:
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else {

            trace("GET_CLOSURE\n");
            uint16_t n_raw = p.get<uint16_t>();
            uint16_t n = htons(n_raw);
            uint16_t n_start = n;
            for (; n > 0; n--) {
                trace("GET_CLOSURE_GET %d\n", n);
                StgClosure *ptr = UNTAG_CLOSURE((StgClosure *) p.get<uint64_t>());
                trace("GET_CLOSURE_LEN %d/%d\n", n, n_start);
                trace("WORD_SIZE %lu\n", WORD_SIZE);
                trace("CLOSURE_SIZE_PTR %p\n", ptr);
                trace("CLOSURE_SIZE %u\n", closure_sizeW(ptr));
                trace("CLOSURE_TYPE %d\n", ptr->header.info->type);

                size_t len = closure_sizeW(ptr) * WORD_SIZE;
                uint32_t len_payload = htonl(len);
                trace("GET_CLOSURE_WRITE1 %lu\n", len);
                resp.write(len_payload);
                trace("GET_CLOSURE_WRITE2 %d\n", n);
                resp.write((const char *) ptr, len);
            }
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_GET_STACK:
        {
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else {
            StgClosure *ptr = UNTAG_CLOSURE((StgClosure *) p.get<uint64_t>());
            trace("STACK_GET %p\n", ptr);
            trace("STACK_SIZE %u\n", closure_sizeW(ptr));
            StgStack *s = ((StgStack *) ptr);


            size_t len = ((s->stack_size + s->stack) - s->sp) * WORD_SIZE;
            uint32_t len_payload = htonl(len);
            trace("GET_CLOSURE_WRITE1 %lu\n", len);
            resp.write(len_payload);
            resp.write((const char *) s->sp, len);
            }
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_GET_INFO_TABLES:
        // TODO: Info tables are immutable so we needn't pause for this request
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else {

            trace("GET_INFO_TABLES\n");
            uint16_t n_raw = p.get<uint16_t>();
            uint16_t n = htons(n_raw);
            for (; n > 0; n--) {
                trace("GET_INFO_GET %d\n", n);
                StgInfoTable *ptr_end = (StgInfoTable *) p.get<uint64_t>();
                // TODO this offset is wrong sometimes
                // You have to subtract 1 so that you get the pointer to the
                // start of the info table.
                StgInfoTable *info = ptr_end - 1;
                trace("INFO_TABLE_SIZE %lu\n", INFO_TABLE_SIZE);
                trace("INFO_TABLE_PTR %p\n", info);

                size_t len = INFO_TABLE_SIZE;
                uint32_t len_payload = htonl(len);
                trace("GET_CLOSURE_WRITE1 %lu\n", len);
                resp.write(len_payload);
                resp.write((const char *) info, len);
            }
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_GET_BITMAP:
        {
            response_code code = RESP_OKAY;
            StgInfoTable *ptr_end = (StgInfoTable *) p.get<uint64_t>();
            // TODO this offset is wrong sometimes
            // You have to subtract 1 so that you get the pointer to the
            // start of the info table.
            StgInfoTable *info = ptr_end - 1;
            switch (info->type) {
              case CATCH_STM_FRAME:
              case CATCH_RETRY_FRAME:
              case ATOMICALLY_FRAME:
              case UNDERFLOW_FRAME:
              case STOP_FRAME:
              case CATCH_FRAME:
              case RET_SMALL:
              {
                  // Small bitmap
                  StgWord bitmap = BITMAP_BITS(info->layout.bitmap);
                  StgWord size   = BITMAP_SIZE(info->layout.bitmap);
                  uint32_t size_payload;
                  size_payload=htonl(size);
                  trace("SIZE %d", size);
                  resp.write((uint32_t) size_payload);
                  while (size > 0) {
                      resp.write((uint8_t) ! (bitmap & 1));
                      bitmap = bitmap >> 1;
                      size--;
                  }
                  break;
              }

              case RET_BCO:
              {
                  StgBCO *bco = (StgBCO *) 0; // TODO: ugh
                  write_large_bitmap(resp, BCO_BITMAP(bco), BCO_BITMAP_SIZE(bco));
                  break;
              }

              case RET_BIG:
              {
                  StgLargeBitmap *bitmap = GET_LARGE_BITMAP(info);
                  write_large_bitmap(resp, bitmap, bitmap->size);
                  break;
              }

              default:
                  code = RESP_BAD_COMMAND;
            }
            resp.finish(code);
            break;
        }

      case CMD_POLL:
        r_poll_pause_resp = &resp;
        // NOTE: Don't call finish so that the process blocks waiting for
        // a response. We will send the response when the process pauses.
        break;

      case CMD_SAVED_OBJECTS:
        int i;
        for (i = 0; i < g_savedObjectState.n_objects; i++) {
          StgStablePtr v = g_savedObjectState.objects[i];
          resp.write((uint64_t)(UNTAG_CLOSURE((StgClosure *)deRefStablePtr(v))));
        }
        resp.finish(RESP_OKAY);
        break;

      //case CMD_FIND_PTR:
      //  trace("FIND_PTR\n");
      //  StgClosure *ptr;
      //  ptr = UNTAG_CLOSURE((StgClosure *) p.get<uint64_t>());
      //  trace("FIND_PTR %p\n", ptr);
      //  trace("FIND_PTR_SIZE %u\n", closure_sizeW(ptr));
      //  findPtrCb(&collect_misc_callback, &resp, (P_) ptr);
      //  resp.finish(RESP_OKAY);
      //  break;

      case CMD_CON_DESCR:
        {
        trace("CON_DESCR\n");
        StgClosure *ptr_end = (StgClosure *) p.get<uint64_t>();
        trace("CON_DESC2 %p\n", ptr_end);
        const char * con_desc = GET_CON_DESC(get_con_itbl(UNTAG_CLOSURE(ptr_end)));
        trace("CON_DESC: %p %lu\n", con_desc, strlen(con_desc));
        write_string(resp, con_desc);
        resp.finish(RESP_OKAY);
        break;
        }
      case CMD_SOURCE_INFO:
        {
        trace("SOURCE_INFO\n");
        StgInfoTable *info_table = (StgInfoTable *) p.get<uint64_t>();
        trace("INFO: %p\n", info_table);
        InfoProvEnt * elt = lookupIPE(info_table);
        trace("ELT: %p\n", info_table);
        uint32_t len_payload;
        if (!elt){
          trace("NOT FOUND\n");
          resp.write((uint32_t) 0);
        }
        else {
          InfoProv ip = elt->prov;
          trace("FOUND\n");

          size_t len = 6;
          uint32_t len_payload = htonl(len);
          resp.write(len_payload);

        //  Using the function just produces garbage.. no idea why
        write_string(resp, ip.table_name);
        write_string(resp, ip.closure_desc);
        write_string(resp, ip.ty_desc);
        write_string(resp, ip.label);
        write_string(resp, ip.module);
        write_string(resp, ip.srcloc);
        }
        resp.finish(RESP_OKAY);
        break;
      }
      case CMD_BLOCKS:
        {
        uint32_t g, n;
        generation * gen;
        bdescr * bd;
        CapabilityPublic ** cap;
        cap = (CapabilityPublic **) capabilities;
        trace("GENERATIONS %d\n", RtsFlags.GcFlags.generations);

        for (n = 0; n < n_capabilities; n ++){
          bd = cap[n]->r.rNursery->blocks;
          write_blocks(resp, bd);

        }

        for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
          gen = &generations[g];
          printf("GEN %d - %lu\n", g, generations[g].n_blocks);
          printf("GEN %d - %lu\n", g, generations[g].n_old_blocks);
          bd = generations[g].blocks;
          printf("BD_START %p\n", bd);
          printf("BD_START_OLD %p\n", generations[g].old_blocks);
          write_blocks(resp,generations[g].blocks);
          write_blocks(resp,generations[g].large_objects);
          write_blocks(resp,generations[g].compact_objects);

        }
        resp.finish(RESP_OKAY);
        break;
        }

      case CMD_BLOCK:
        {
        StgClosure *ptr = UNTAG_CLOSURE((StgClosure *) p.get<uint64_t>());
        bdescr * bd = (bdescr *) (((W_) ptr) & ~BLOCK_MASK);
        trace("BD_ADDR: %p, %p", ptr,bd);
        write_block(resp, bd);
        resp.finish(RESP_OKAY);
        break;
        }


      default:
        return 1;
    }
    return 0;
}


static void handle_connection(const unsigned int sock_fd) {
    Socket sock(sock_fd);
    char *buf = new char[MAX_CMD_SIZE];
    while (true) {
        uint32_t cmdlen_n, cmdlen;

        sock.read((char *)&cmdlen_n, 4);
        cmdlen = ntohl(cmdlen_n);

        cmdlen = ntohl(cmdlen_n);
        char *large_buf = buf;
        bool use_large_buf = cmdlen > MAX_CMD_SIZE;
        if (use_large_buf) {
          large_buf = new char[cmdlen];
        }

        trace("LEN: %d\n", cmdlen);
        sock.read(large_buf, cmdlen);
        trace("CONT:%s\n", buf);
        try {
            trace("LEN2: %d\n", cmdlen);
            handle_command(sock, large_buf, cmdlen);
        } catch (Parser::EndOfInput e) {
            barf("error");
            Response resp(sock);
            resp.finish(RESP_BAD_COMMAND);
        }

        if (use_large_buf) {
          delete[] large_buf;
        }
    }
    delete[] buf;
}

/* return non-zero on error */
/*
static void handle_connection(const unsigned int sock_fd) {
    Socket sock(sock_fd);
    char *buf = new char[MAX_CMD_SIZE];
    while (true) {
        uint32_t cmdlen_n, cmdlen;

        sock.read((char *)&cmdlen_n, 4);
        cmdlen = ntohl(cmdlen_n);

        trace("LEN: %d\n", cmdlen);
        sock.read(buf, cmdlen);
        trace("CONT:%s\n", buf);
        try {
            trace("LEN2: %d\n", cmdlen);
            handle_command(sock, buf, cmdlen);
        } catch (Parser::EndOfInput e) {
            barf("error");
            Response resp(sock);
            resp.finish(RESP_BAD_COMMAND);
        }
    }
    delete[] buf;
}
*/

extern "C"
void start(const char* socket_path) {
    trace("starting\n");
    struct sockaddr_un local, remote;

    if (strlen(socket_path) >= sizeof(local.sun_path)) {
        barf("socket_path too long: \"%s\"", socket_path);
    }

    int s = socket(AF_UNIX, SOCK_STREAM, 0);
    if (s == -1) {
        barf("socket failed");
    }

    // Bind socket
    {
        local.sun_family = AF_UNIX;
        strncpy(local.sun_path, socket_path, sizeof(local.sun_path));
        unlink(local.sun_path);
        if (bind(s, (struct sockaddr *) &local, sizeof(local)) != 0) {
            barf("bind failed");
        }
    }

    if (listen(s, 1) != 0) {
        barf("listen failed");
    }
    fflush(stdout);
    while (true) {
        socklen_t len;
        int s2 = accept(s, (struct sockaddr *) &remote, &len);
        if (s2 == -1) {
          barf("accept failed %d", s2);
        }
        handle_connection(s2);
    }
}

extern "C"
StgWord saveClosures(StgWord n, HsStablePtr *sps)
{
    struct savedObjectsState *ps = &g_savedObjectState;
    StgWord i;

    if(n > maxSavedObjects)
        return maxSavedObjects;

    for (i = 0; i < n; i++) {
        ps->objects[i] = sps[i];
    }
    ps->n_objects = i;
    return 0;
}

