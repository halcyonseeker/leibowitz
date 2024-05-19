
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "utils.h"

void *
xcalloc(size_t nmemb, size_t size, char *extra_msg)
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
xstrtol(const char *nptr, char **endptr, int base, char *extra_msg)
{
	long r = strtol(nptr, endptr, base);
	if (errno) {
		perror("strtol");
		if (extra_msg) fprintf(stderr, "%s\n", extra_msg);
		exit(1);
	}
	return r;
}
