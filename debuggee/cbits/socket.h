#pragma once

#include <string>
#include <sys/types.h>

class SocketError {
  private:
    int err_no;
    std::string what;
  public:
    SocketError(int err_no, std::string what);
};

class Socket {
  private:
    const int fd;
  public:
    Socket(int fd);
    /* read len bytes into the given buffer */
    void read(char *buf, size_t len);
    void write(const char *buf, size_t len);
};
