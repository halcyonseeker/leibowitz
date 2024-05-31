LISP_SOURCES_REGEX = *.asd *.lisp */*.asd */*.lisp */*/*.lisp
TARGETS = build/leibowitz build/_leibowitz build/leibowitz.1
PREFIX ?= /usr/local

all: $(TARGETS)

build/leibowitz: $(LISP_SOURCES_REGEX)
	mkdir -p build
	sbcl --noinform --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz) (asdf:make :leibowitz) (quit))'

build/leibowitz.1: build/leibowitz
	build/leibowitz --mandoc-documentation > build/leibowitz.1

build/_leibowitz: build/leibowitz
	build/leibowitz --zsh-completions > build/_leibowitz
	@ # I install to the ~/.local prefix for testing but zsh
	@ # doesn't search any of the home directory by default.  This
	@ # line loads the completions.
	@ echo Now run this in order to load the completions:
	@ echo 'export fpath=(~/.local/share/zsh/site-functions $$fpath); autoload -U compinit; compinit'

.PHONY: test install uninstall clean

test: $(LISP_SOURCES_REGEX)
	sbcl --noinform --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz/tests) (asdf:test-system :leibowitz) (quit))'

install: $(TARGETS)
	install -D -m 0755 build/leibowitz   $(PREFIX)/bin/leibowitz
	install -D -m 0644 build/_leibowitz  $(PREFIX)/share/zsh/site-functions/_leibowitz
	install -D -m 0644 build/leibowitz.1 $(PREFIX)/man/man1/leibowitz.1

uninstall:
	rm -f $(PREFIX)/bin/leibowitz
	rm -f $(PREFIX)/share/zsh/site-functions/_leibowitz
	rm -f $(PREFIX)/man/man1/leibowitz.1

clean:
	rm -rf build
