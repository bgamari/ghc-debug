#pragma once

void trace(const char *fmt, ...)
    __attribute__((format (PRINTF, 1, 2)));
