ERR_FLAGS = -Wall -Werror -Wpedantic -Wextra -Wformat=2 -Wno-unused-parameter \
	-Wshadow -Wwrite-strings -Wstrict-prototypes -Wold-style-definition \
	-Wredundant-decls -Wnested-externs -Wmissing-include-dirs
SAN_FLAGS = -fsanitize=address -fsanitize=alignment -fsanitize=bool \
	-fsanitize=bounds -fsanitize=enum -fsanitize=float-cast-overflow \
	-fsanitize=float-divide-by-zero -fsanitize=integer-divide-by-zero \
	-fsanitize=nonnull-attribute -fsanitize=null -fsanitize=object-size \
	-fsanitize=vla-bound -fsanitize=returns-nonnull-attribute \
	-fsanitize=shift -fsanitize=signed-integer-overflow -fsanitize=vptr \
	-fsanitize=unreachable -fsanitize=pointer-overflow -fsanitize=return

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
