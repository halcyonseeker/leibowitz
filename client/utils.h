
#ifndef __UTILS
#define __UTILS

#define INFO(...)                                     \
	do {                                          \
		printf("\033[1;38;5;6mINFO\033[m: "); \
		printf(__VA_ARGS__);                  \
	} while (0)


#define WARN(...)						\
	do {							\
		fprintf(stderr, "\033[1;38;5;1mFATAL\033[m: ");	\
		fprintf(stderr, __VA_ARGS__);			\
	} while (0)

#endif	/* __UTILS */
