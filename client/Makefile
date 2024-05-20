CC ?= cc
CFLAGS += -std=c11 -O2
ERR_FLAGS = -Wall -Werror -Wpedantic -Wextra -Wformat=2 -Wno-unused-parameter \
	-Wshadow -Wwrite-strings -Wstrict-prototypes -Wold-style-definition \
	-Wredundant-decls -Wnested-externs -Wmissing-include-dirs -Wlogical-op \
	-Wjump-misses-init -Wlong-long -Wno-unknown-warning-option
SAN_FLAGS = -fsanitize=address -fsanitize=alignment -fsanitize=bool \
	-fsanitize=bounds -fsanitize=enum -fsanitize=float-cast-overflow \
	-fsanitize=float-divide-by-zero -fsanitize=integer-divide-by-zero \
	-fsanitize=nonnull-attribute -fsanitize=null -fsanitize=object-size \
	-fsanitize=vla-bound -fsanitize=returns-nonnull-attribute \
	-fsanitize=shift -fsanitize=signed-integer-overflow -fsanitize=vptr \
	-fsanitize=unreachable -fsanitize=pointer-overflow -fsanitize=return
CFLAGS += $(ERR_FLAGS) $(SAN_FLAGS)

all: leibowitz-client

utils.o: utils.c utils.h
	$(CC) -c -o $@ $(CFLAGS) $<

asprintf.o: asprintf.c asprintf.h
	$(CC) -c -o $@ $(CFLAGS) $<

leibowitz-client.o: leibowitz-client.c
	$(CC) -c -o $@ $(CFLAGS) $<

leibowitz-client: leibowitz-client.o utils.o asprintf.o
	$(CC) -o $@ $(CFLAGS) $^

clean:
	rm -f leibowitz-client vgcore.* *.core *.o
