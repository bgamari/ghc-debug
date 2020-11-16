#include <stdarg.h>
#include <stdio.h>

#ifndef TRACE
void trace(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}
#else
void trace(const char * fmt, ...){
  (void) fmt;
}
#endif

