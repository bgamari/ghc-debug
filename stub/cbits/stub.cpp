#include <cerrno>
#include <cstdint>
#include <cstdbool>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/wait.h>
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

#if !defined(TABLES_NEXT_TO_CODE)
#error TABLES_NEXT_TO_CODE not defined
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

namespace {
    bool isFUN(StgHalfWord flags) {
        return !isTHUNK(flags) && hasSRT(flags);
    }
#ifndef ip_STACK_FRAME
    // ghc has this now, but it's new
    bool ip_STACK_FRAME(StgInfoTable* info) {
      switch(info->type) {
      case RET_SMALL:
      case RET_BIG:
      case RET_FUN:
      case UPDATE_FRAME:
      case CATCH_FRAME:
      case UNDERFLOW_FRAME:
      case STOP_FRAME:
      case ATOMICALLY_FRAME:
      case CATCH_RETRY_FRAME:
      case CATCH_STM_FRAME: return true;
      default: return false;
      }
    }
#endif
}

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
    CMD_BLOCKS = 14,
    CMD_BLOCK = 15,
    CMD_FUN_BITMAP = 16,
    CMD_GET_SRT = 17
};

enum response_code {
    RESP_OKAY = 0,
    RESP_OKAY_CONTINUES = 1,
    // Error responses
    RESP_BAD_COMMAND = 0x100,
    RESP_BAD_STACK = 0x104,
    RESP_ALREADY_PAUSED = 0x101,
    RESP_NOT_PAUSED = 0x102,
    RESP_NO_RESUME = 0x103,
};

extern "C" Capability **capabilities;

const int maxSavedObjects = 20;

// Whether to fork on pause or not.
static bool use_fork = false;

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
            trace("bytes in response payload: %lu\n", len);
            uint32_t len_payload;
            uint16_t status_payload;
            len_payload=htonl(len);
            status_payload = htons(status);
            trace("responding with status: %d\n", status);
            // Header is the length
            this->sock.write((char *) &len_payload, sizeof(uint32_t));
            // Then status
            this->sock.write((char *) &status_payload, sizeof(uint16_t));
            // then the body, usually empty
            trace("responding with body of length %lu: ( ", len);
            for (int i = 0; i < len; i++)
            {
                trace("%02X", buf[i]);
            }
            trace(" )\n");
            this->sock.write(this->buf, len);
            this->tail = this->buf;
            trace("response written to socket\n\n");
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
            trace("LEN TOO BIG %lu, %lu\n", len, this->buf_size);
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
        trace("finishing with code: %d\n", status);
        this->flush(status);
    }
};

static bool paused = false;
static PauseToken * r_paused;
static Response * r_poll_pause_resp = NULL;

static StgStablePtr rts_saved_closure = NULL;


extern "C"
void pause_mutator() {
  trace("pausing mutator\n");
  pid_t pid;
  if (use_fork){
    pid = fork();
  } else {
    pid = 0;
  }
  // Only pause the child process, the parent blocks until the child has finished.
  if (pid == 0){
    r_paused = rts_pause();
    if (r_poll_pause_resp != NULL){
        r_poll_pause_resp->finish(RESP_OKAY);
    }
    paused = true;
  }
  else {
    int status = 0;
    wait(&status);
    use_fork = false;
  }
}

extern "C"
void resume_mutator() {
  trace("resuming mutator\n");
  if (use_fork){
    trace("exiting child\n");
    // Exit, the parent is blocked until we are finished.
    exit(0);
  } else {
    trace("resuming rts\n");
    rts_resume(r_paused);
    paused = false;
  }
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

// Size fields are always uint32_t in network-byte-order.
static void write_size(Response& resp, StgWord size) {
    uint32_t size_payload = htonl(size);
    trace("SIZE %llu", size);
    resp.write((uint32_t) size_payload);
}

static void write_large_bitmap(Response& resp, StgLargeBitmap *large_bitmap, StgWord size) {
    uint32_t b = 0;
    write_size(resp, size);

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

static void write_small_bitmap(Response& resp, StgWord bitmap, StgWord size) {
    uint32_t i = 0;

    // Small bitmap
    write_size(resp, size);
    while (size > 0) {
        resp.write((uint8_t) ! (bitmap & 1));
        bitmap = bitmap >> 1;
        size--;
    }
}

static void write_fun_bitmap(Response& resp, StgWord size, StgClosure * fun){

    const StgFunInfoTable *fun_info;
    fun_info = get_fun_itbl(UNTAG_CONST_CLOSURE(fun));
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        write_small_bitmap
            (resp, BITMAP_BITS(fun_info->f.b.bitmap), size);
        break;
    case ARG_GEN_BIG:
        write_large_bitmap(resp, GET_FUN_LARGE_BITMAP(fun_info), size);
        break;
    default:
        write_small_bitmap(resp, BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]), size);
        break;
    }
}


static void write_string(Response& resp, const char * s){
    StgWord size = strlen(s);
    write_size(resp, size);
    trace("WRITING: %s\n", s);
    resp.write(s, size);
}


static void write_block(Response * resp, bdescr * bd){
  bdescr * real_bd;
  // TODO, need a special case here as well for LARGE objects?
  if (bd->flags & BF_COMPACT){
    bdescr *object_block, *head_block;

    object_block = bd;

    ASSERT((object_block->flags & BF_COMPACT) != 0);

    if (object_block->blocks == 0)
        head_block = object_block->link;
    else
        head_block = object_block;

    ASSERT((head_block->flags & BF_COMPACT) != 0);

    real_bd = head_block;
  } else {
    real_bd = bd;
  }
  resp->write(real_bd->flags);
  resp->write(real_bd->start);
  uint32_t len_payload = htonl(BLOCK_SIZE * real_bd->blocks);
  resp->write(len_payload);
  resp->write((const char *) real_bd->start, BLOCK_SIZE * real_bd->blocks);
}

static void write_blocks(Response * resp, bdescr * bd){
    for (; bd != NULL; bd = bd->link){
      write_block(resp, bd);
    }
}

void list_blocks_callback(void *user, bdescr * bd){
    write_blocks((Response *) user, bd);
}

/* return non-zero on error */
static int handle_command(Socket& sock, const char *buf, uint32_t cmd_len) {
    trace("handling command of length: %d\n", cmd_len);
    Parser p(buf, cmd_len);
    Response resp(sock);
    uint32_t cmd = ntohl(p.get<uint32_t>());
    trace("read CommandId: %d\n", cmd);
    switch (cmd) {
      case CMD_VERSION:
        uint32_t ver_payload;
        uint32_t ver_payload1;
        ver_payload=htonl(__GLASGOW_HASKELL__) ;
        ver_payload1=htonl(__GLASGOW_HASKELL_PATCHLEVEL1__) ;
        resp.write(ver_payload);
        resp.write(ver_payload1);
        resp.finish(RESP_OKAY);
        break;

      case CMD_PAUSE:
        trace("handling pause command\n");
        trace("paused already?: %s\n", paused ? "yes" : "no");
        use_fork = (bool)ntohl(p.get<uint8_t>());
        if (paused) {
            resp.finish(RESP_ALREADY_PAUSED);
        } else {
            trace("fork?: %s\n", use_fork ? "yes" : "no");
            pause_mutator();
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_RESUME:
        trace("handling resume command\n");
        trace("already running?: %s\n", paused ? "no" : "yes");
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

      case CMD_GET_SRT:
        // TODO: SRTs are immutable so we needn't pause for this request
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else {
            trace("GET_SRT\n");
            uint16_t n_raw = p.get<uint16_t>();
            uint16_t n = htons(n_raw);
            for (; n > 0; n--) {
                trace("GET_SRT_GET_INFO %d\n", n);
                StgInfoTable *ptr_end = (StgInfoTable *) p.get<uint64_t>();
                StgInfoTable *info = INFO_PTR_TO_STRUCT(ptr_end);
                StgHalfWord flags = ipFlags(info);
                StgClosure* srt = nullptr;
                if(ip_SRT(info) && info->srt) { // Is this info table of a type which CAN have an SRT, and does it actually have an SRT?
                  if (isFUN(flags)) {
                    StgFunInfoTable* funinfo = FUN_INFO_PTR_TO_STRUCT(ptr_end);
                    srt = GET_FUN_SRT(funinfo);
                  } else if (isTHUNK(flags)) {
                    StgThunkInfoTable* thunkinfo = THUNK_INFO_PTR_TO_STRUCT(ptr_end);
                    srt = GET_SRT(thunkinfo);
                  } else if (ip_STACK_FRAME(info)) {
                    StgRetInfoTable* retinfo = RET_INFO_PTR_TO_STRUCT(ptr_end);
                    srt = GET_SRT(retinfo);
                  }
                }
                resp.write((uint64_t)srt);
            }
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_GET_BITMAP:
        {
            response_code code = RESP_OKAY;
            StgClosure *s = (StgClosure *) p.get<uint64_t>();
            uint32_t o = ntohl(p.get<uint32_t>());
            // TODO this offset is wrong sometimes
            // You have to subtract 1 so that you get the pointer to the
            // start of the info table.
            StgClosure *c = (StgClosure *)((uint64_t (s)) + ((uint64_t) o));
            trace("BITMAP %p %d %p\n", s, o, c);
            const StgInfoTable *info = get_itbl(c);
            switch (info->type) {
              case CATCH_STM_FRAME:
              case CATCH_RETRY_FRAME:
              case ATOMICALLY_FRAME:
              case UNDERFLOW_FRAME:
              case STOP_FRAME:
              case CATCH_FRAME:
              case UPDATE_FRAME:
              case RET_SMALL:
              {
                  // Small bitmap
                  StgWord bitmap = BITMAP_BITS(info->layout.bitmap);
                  StgWord size   = BITMAP_SIZE(info->layout.bitmap);
                  write_small_bitmap(resp, bitmap, size);
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
              case RET_FUN:
              {
                StgRetFun *ret_fun;

                ret_fun = (StgRetFun *)c;
                StgWord size = ret_fun->size;
                write_fun_bitmap(resp, size, ret_fun->fun);
                break;
              }

              default:
                  trace("INFO %p %d", info, info->type);
                  code = RESP_BAD_STACK;
            }
            resp.finish(code);
            break;
        }

      case CMD_FUN_BITMAP:
        {
          StgClosure *fun = (StgClosure *) p.get<uint64_t>();
          uint16_t n_raw = p.get<uint16_t>();
          uint16_t n = htons(n_raw);
          write_fun_bitmap(resp, n, fun);
        }
        resp.finish(RESP_OKAY);
        break;


      case CMD_POLL:
        r_poll_pause_resp = new Response(resp);
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
        StgConInfoTable *ptr_end = (StgConInfoTable *) p.get<uint64_t>();
        trace("CON_DESC2 %p\n", ptr_end);
        const char * con_desc = GET_CON_DESC(ptr_end - 1);
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
          write_size(resp, len);

          //  Using the function just produces garbage.. no idea why
          write_string(resp, ip.table_name);
          write_string(resp, ip.closure_desc);
          write_string(resp, ip.ty_desc);
          write_string(resp, ip.label);
          write_string(resp, ip.module);
#if MIN_VERSION_GLASGOW_HASKELL(9,5,0,0)
          {
            size_t len_file = strlen(ip.src_file);
            size_t len_span = strlen(ip.src_span);
            write_size(resp, len_file + 1 + len_span);
            resp.write(ip.src_file, len_file);
            resp.write(":", 1);
            resp.write(ip.src_span, len_span);
          }
#else
          write_string(resp, ip.srcloc);
#endif
        }
        resp.finish(RESP_OKAY);
        break;
      }
      case CMD_BLOCKS:
        {
        listAllBlocks(list_blocks_callback, (void *) &resp);
        resp.finish(RESP_OKAY);
        break;
        }

      case CMD_BLOCK:
        {
        // TODO: This doesn't work correctly for BF_NONMOVING blocks
        // For those blocks you need to apply the NONMOVING_SEGMENT_MASK
        // in order to find the start of the block.
        bdescr *bd = Bdescr ((P_) p.get<uint64_t>());
        trace("BD_ADDR: %p", bd);
        write_block(&resp, bd);
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
        trace("handler is waiting for a request\n");
        uint32_t cmdlen_n, cmdlen;

        size_t n_read = sock.read((char *)&cmdlen_n, 4);

        // If the read returns 0, consider it a disconnect
        if (n_read == 0) {
            trace("handler is done\n");
            return;
        }

        cmdlen = ntohl(cmdlen_n);

        char *large_buf = buf;
        bool use_large_buf = cmdlen > MAX_CMD_SIZE;
        if (use_large_buf) {
          large_buf = new char[cmdlen];
        }

        trace("reading cmd of length: %d\n", cmdlen);
        sock.read(large_buf, cmdlen);
        trace("leftover: %s\n", buf);
        try {
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
void start_over_tcp(const char* socket_addr, uint16_t port) {
    trace("starting with socket: %s:%d\n", socket_addr, port);
    struct sockaddr_in local, remote;
    int family;

    // try ipv4
    if (inet_pton(AF_INET, socket_addr, &local.sin_addr) == 1) {
        family = AF_INET;
    } else {
        // try ipv6
        if (inet_pton(AF_INET6, socket_addr, &local.sin_addr) == 1) {
            family = AF_INET6;
        } else {
            barf("invalid socket address: \"%s\"", socket_addr);
        }
    }

    // Open the socket for listening
    int listenHdl = socket(family, SOCK_STREAM, 0);
    if (listenHdl == -1) {
        barf("socket failed");
    }

    // Bind the socket to an address
    local.sin_family = family;
    local.sin_port = htons(port);
    if (bind(listenHdl, (struct sockaddr *) &local, sizeof(local)) != 0) {
        barf("bind failed");
    }

    // Listen for connections
    if (listen(listenHdl, 1) != 0) {
        barf("listen failed");
    }
    fflush(stdout);

    while (true) {
        // Wait for client connection
        socklen_t len = sizeof(remote);
        int commHdl = accept(listenHdl, (struct sockaddr *) &remote, &len);
        if (commHdl == -1) {
            barf("accept failed %s", strerror(errno));
        }

        // Handle and on disconnect listen for more connections
        handle_connection(commHdl);
    }
}

extern "C"
void start_over_un(const char* socket_path) {
    trace("starting with socket path: %s\n", socket_path);
    struct sockaddr_un local, remote;

    if (strlen(socket_path) >= sizeof(local.sun_path)) {
        barf("socket_path too long: \"%s\"", socket_path);
    }

    // Open the socket for listening
    int listenHdl = socket(AF_UNIX, SOCK_STREAM, 0);
    if (listenHdl == -1) {
        barf("socket failed");
    }

    // Bind the socket to an address
    local.sun_family = AF_UNIX;
    strncpy(local.sun_path, socket_path, sizeof(local.sun_path));
    unlink(local.sun_path);
    if (bind(listenHdl, (struct sockaddr *) &local, sizeof(local)) != 0) {
        barf("bind failed");
    }

    // Listen for connections
    if (listen(listenHdl, 1) != 0) {
        barf("listen failed");
    }
    fflush(stdout);

    while (true) {
        // Wait for client connection
        socklen_t len = sizeof(remote);
        int commHdl = accept(listenHdl, (struct sockaddr *) &remote, &len);
        if (commHdl == -1) {
            barf("accept failed %s", strerror(errno));
        }

        // Handle and on disconnect listen for more connections
        handle_connection(commHdl);
    }
}

extern "C"
StgWord saveClosures(StgWord n, HsStablePtr *sps)
{
    StgWord i;

    if(n > maxSavedObjects)
        return maxSavedObjects;

    for (i = 0; i < n; i++) {
        g_savedObjectState.objects[i] = sps[i];
    }
    g_savedObjectState.n_objects = i;
    return 0;
}

