
#ifndef __UTILS
#define __UTILS

#include <stdlib.h>

#define INFO(...)                                     \
	do {                                          \
		printf("\033[1;38;5;6mINFO\033[m: "); \
		printf(__VA_ARGS__);                  \
	} while (0)


#define WARN(...)                                               \
	do {                                                    \
		fprintf(stderr, "\033[1;38;5;1mWARN\033[m: ");  \
		fprintf(stderr, __VA_ARGS__);                   \
	} while (0)

extern void *xcalloc(size_t nmemb, size_t size, const char *const extra_msg);


extern long xstrtol(const char *const nptr, char **endptr, int base,
    const char *const extra_msg);



#endif	/* __UTILS */
