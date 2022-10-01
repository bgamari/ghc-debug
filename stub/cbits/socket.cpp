#include "socket.h"
#include <unistd.h>
#include <Rts.h>

SocketError::SocketError(int err_no, std::string what)
  : err_no(err_no), what(what) { }

Socket::Socket(int fd)
  : fd(fd) { }

size_t Socket::read(char *buf, size_t len) {
    int n_read = 0;
    while (len > 0) {
        ssize_t ret = ::read(this->fd, (void *) buf, len);
        if (ret < 0) {
            throw SocketError(errno, "read");
        } else if (ret == 0) {
            // if read() returns 0 it should be considered a disconnect
            return 0;
        }
        len -= ret;
        buf += ret;
        n_read += ret;
    }
    return n_read;
}

void Socket::write(const char *buf, size_t len) {
//    debugBelch("WRITING %s %d\n", buf, len);
//    for (int i = 0; i < len; i++)
//    {
//      debugBelch("%02X", buf[i]);
//    }
//    debugBelch("\n");
    while (len > 0) {
        ssize_t ret = ::write(this->fd, buf, len);
        if (ret < 0) {
            throw SocketError(errno, "write");
        }
        len -= ret;
        buf += ret;
    }
}

