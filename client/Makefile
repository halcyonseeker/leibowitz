CFLAGS = -Wall -Werror -Wpedantic
CC ?= cc
SRC = *c

all: leibowitz-client

leibowitz-client: $(SRC)
	$(CC) -o $@ $(CFLAGS) $<
