CFLAGS = -Wall -Werror -Wpedantic
CC ?= cc
SOURCES = *.c *.h

all: leibowitz-client

leibowitz-client: $(SOURCES)
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f leibowitz-client
