
#ifndef __ASPRINTF_H
#define __ASPRINTF_H

#include <stdarg.h>

int asprintf(char **s, const char *fmt, ...);
int vasprintf(char **s, const char *fmt, va_list ap);

#endif	/* __ASPRINTF_H */
