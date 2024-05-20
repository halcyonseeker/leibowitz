ERR_FLAGS = -Wall -Werror -Wpedantic -Wextra -Wformat=2 -Wno-unused-parameter \
	-Wshadow -Wwrite-strings -Wstrict-prototypes -Wold-style-definition \
	-Wredundant-decls -Wnested-externs -Wmissing-include-dirs
SAN_FLAGS = -fsanitize=address -fsanitize=undefined
CC ?= cc
SOURCES = *.c *.h

# GCC warnings that Clang doesn't provide:
ifeq ($(CC),gcc)
	ERR_FLAGS += -Wjump-misses-init -Wlogical-op
endif

CFLAGS += $(ERR_FLAGS) $(SAN_FLAGS)

all: leibowitz-client

leibowitz-client: $(SOURCES)
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f leibowitz-client
