#include "socket.h"
#include <unistd.h>
#include <Rts.h>

SocketError::SocketError(int err_no, std::string what)
  : err_no(err_no), what(what) { }

Socket::Socket(int fd)
  : fd(fd) { }

void Socket::read(char *buf, size_t len) {
    debugBelch("%d\n", this->fd);
    while (len > 0) {
        ssize_t ret = ::read(this->fd, (void *) buf, len);
        if (ret < 0) {
            debugBelch("%d\n", errno);
            throw SocketError(errno, "read");
        }
        len -= ret;
        buf += ret;
    }
}

void Socket::write(const char *buf, size_t len) {
    debugBelch("WRITING %s %d\n", buf, len);
    for (int i = 0; i < len; i++)
    {
      debugBelch("%02X", buf[i]);
    }
    debugBelch("\n");
    while (len > 0) {
        ssize_t ret = ::write(this->fd, buf, len);
        if (ret < 0) {
            throw SocketError(errno, "write");
        }
        len -= ret;
        buf += ret;
    }
}

