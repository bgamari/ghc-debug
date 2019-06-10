#pragma once

#include <unistd.h>

class Parser {
  private:
    const char *buf;
    size_t remaining;

  public:
    inline Parser(const char *buf, size_t len) : buf(buf), remaining(len) { }

    class EndOfInput {};

    inline bool end() const {
        return this->remaining == 0;
    }

    inline size_t available() const {
        return this->remaining;
    }

    template<typename T>
    T get() {
        if (this->remaining < sizeof(T)) {
            throw EndOfInput();
        } else {
            T x = *(T *) this->buf;
            this->buf += sizeof(T);
            this->remaining -= sizeof(T);
            return x;
        }
    }
};

