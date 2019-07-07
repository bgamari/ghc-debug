#include <cerrno>
#include <cstdint>
#include <cstdbool>
#include <cstring>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <arpa/inet.h>

#include <thread>
#include <functional>

#include <Rts.h>
#include "socket.h"
#include "parser.h"

#define MAX_CMD_SIZE 4096


#define WORD_SIZE sizeof(unsigned long)

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
    CMD_GET_INFO_TABLES = 6
};

enum response_code {
    RESP_OKAY = 0,
    RESP_OKAY_CONTINUES = 1,
    // Error responses
    RESP_BAD_COMMAND = 0x100,
    RESP_ALREADY_PAUSED = 0x101,
    RESP_NOT_PAUSED = 0x102,
};

// RTS signatures
// FIXME: These need to be made not private in GHC.
extern "C" {
typedef void (*evac_fn)(void *user, StgClosure **root);
void threadStableNameTable ( evac_fn evac, void *user );
void threadStablePtrTable ( evac_fn evac, void *user );
void stopAllCapabilities (Capability **pCap, Task *task);
void releaseAllCapabilities(uint32_t n, Capability *cap, Task *task);
}

static bool paused = false;
static RtsPaused r_paused;
static Task *task = NULL;

extern "C"
void pause_mutator() {
  printf("pausing c++");
  rts_pause();
  printf("paused c++");
  paused = true;
}

extern "C"
void resume_mutator() {
  printf("resuming c++\n");
  rts_unpause(r_paused);
  paused = false;
}

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
            printf("LEN: %d", len);
            Header *hdr = (Header *) this->buf;
            hdr->len = len;
            hdr->status = status;
            uint32_t len_payload;
            uint16_t status_payload;
            len_payload=htons(len);
            status_payload = htons(status);
            printf("STATUS: %d\n", status);
            // Header is the length
            this->sock.write((char *) &len_payload, sizeof(uint32_t));
            // Then status
            this->sock.write((char *) &status_payload, sizeof(uint16_t));
            // then the body, usually empty
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
        delete this->buf;
    }

    template<typename T>
    void write(T x) {
        write((const char *) &x, sizeof(T));
    }

    void write(const char *buf, size_t len) {
        if (len > this->buf_size) {
            this->flush(RESP_OKAY_CONTINUES);

            Header hdr;
            hdr.len = len;
            hdr.status = RESP_OKAY_CONTINUES;
            this->sock.write((const char *) &hdr, sizeof(Header));
            this->sock.write(buf, len);
        } else {
            if (this->tail + len >= this->buf + this->buf_size) {
                this->flush(RESP_OKAY_CONTINUES);
            }

            memcpy(this->tail, buf, len);
            this->tail += len;
        }
    }

    void finish(enum response_code status) {
        printf("FINISH: %d\n", status);
        this->flush(status);
    }
};

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

void collect_stable_names(std::function<void(StgClosure*)> f) {
    threadStableNameTable((evac_fn) evac_fn_helper, &f);
}

void collect_stable_ptrs(std::function<void(StgClosure*)> f) {
    threadStablePtrTable((evac_fn) evac_fn_helper, &f);
}

/* return non-zero on error */
static int handle_command(Socket& sock, const char *buf, uint32_t cmd_len) {
    printf("HANDLE: %d\n", cmd_len);
    Parser p(buf, cmd_len);
    Response resp(sock);
    printf("P %d\n", p.available());
    uint32_t cmd = ntohl(p.get<uint32_t>());
    printf("CMD: %d\n", cmd);
    switch (cmd) {
      case CMD_VERSION:
        resp.finish(RESP_OKAY);
        break;

      case CMD_PAUSE:
        printf("PAUSE: %d", paused);
        if (paused) {
            printf("ALREADY");
            resp.finish(RESP_ALREADY_PAUSED);
        } else {
            pause_mutator();
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_RESUME:
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else {
            resume_mutator();
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_GET_ROOTS:
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else {
            collect_threads([&](StgTSO *tso) { resp.write((uint64_t) tso); });
            collect_stable_names([&](StgClosure *sn) { resp.write((uint64_t) sn); });
            collect_stable_ptrs([&](StgClosure *sn) { resp.write((uint64_t) sn); });
            resp.finish(RESP_OKAY);
        }
        break;

      case CMD_GET_CLOSURES:
        if (!paused) {
            resp.finish(RESP_NOT_PAUSED);
        } else {
            uint16_t n = p.get<uint16_t>();
            for (; n > 0; n--) {
                StgClosure *ptr = (StgClosure *) p.get<uint64_t>();
                size_t len = closure_sizeW(ptr) * WORD_SIZE;
                resp.write((uint32_t) len);
                resp.write((const char *) ptr, len);
            }
            resp.finish(RESP_OKAY);
        }
        break;

      default:
        return 1;
    }
}

/* return non-zero on error */
static void handle_connection(const unsigned int sock_fd) {
    Socket sock(sock_fd);
    char *buf = new char[MAX_CMD_SIZE];
    while (true) {
        uint32_t cmdlen;

        sock.read((char *)&cmdlen, 4);
        printf("LEN: %d\n", cmdlen);
        sock.read(buf, cmdlen);
        printf("CONT:%s\n", buf);
        try {
            printf("LEN2: %d\n", cmdlen);
            handle_command(sock, buf, cmdlen);
        } catch (Parser::EndOfInput e) {
            barf("error");
            Response resp(sock);
            resp.finish(RESP_BAD_COMMAND);
        }
    }
    delete[] buf;
}

void serve(void) {
    struct sockaddr_un local, remote;

    int s = socket(AF_UNIX, SOCK_STREAM, 0);
    if (s == -1) {
        barf("socket failed");
    }

    // Bind socket
    {
        local.sun_family = AF_UNIX;
        strcpy(local.sun_path, "/tmp/ghc-debug");
        unlink(local.sun_path);
        int len = strlen(local.sun_path) + sizeof(local.sun_family);
        if (bind(s, (struct sockaddr *) &local, len) != 0) {
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

static std::thread *server_thread;

extern "C"
void start(void) {
    printf("starting\n");
    server_thread = new std::thread(serve);
}

