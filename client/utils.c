
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "utils.h"

void *
xcalloc(size_t nmemb, size_t size, const char *const extra_msg)
{
	void *p = calloc(nmemb, size);
	if (!p) {
		perror("calloc");
		if (extra_msg) fprintf(stderr, "%s\n", extra_msg);
		exit(1);
	}
	return p;
}

long
xstrtol(const char *const nptr, char **endptr, const int base,
    const char *const extra_msg)
{
	long r = strtol(nptr, endptr, base);
	if (errno) {
		perror("strtol");
		if (extra_msg) fprintf(stderr, "%s\n", extra_msg);
		exit(1);
	}
	return r;
}
