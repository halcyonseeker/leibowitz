LISP_SOURCES_REGEX = *.asd *.lisp */*.asd */*.lisp */*/*.lisp
PREFIX ?= /usr/local

all: build completions man

build: $(LISP_SOURCES_REGEX)
	mkdir -p build
	sbcl --noinform --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz) (asdf:make :leibowitz) (quit))'

test: $(LISP_SOURCES_REGEX)
	sbcl --noinform --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz/tests) (asdf:test-system :leibowitz) (quit))'

man: build
	build/leibowitz --mandoc-documentation > build/leibowitz.1

completions: build
	build/leibowitz --zsh-completions > build/_leibowitz
	@ # I install to the ~/.local prefix for testing but zsh
	@ # doesn't search any of the home directory by default.  This
	@ # line loads the completions.
	@ echo Now run this in order to load the completions:
	@ echo 'export fpath=(~/.local/share/zsh/site-functions $$fpath); autoload -U compinit; compinit'

install: build completions man
	mkdir -p $(PREFIX)/bin/ $(PREFIX)/share/zsh/site-functions/ $(PREFIX)/man/man1/
	cp build/leibowitz $(PREFIX)/bin
	cp build/_leibowitz $(PREFIX)/share/zsh/site-functions/_leibowitz
	cp build/leibowitz.1 $(PREFIX)/man/man1/leibowitz.1

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/bin/leibowitz
	rm -f $(PREFIX)/share/zsh/site-functions/_leibowitz
	rm -f $(PREFIX)/man/man1/leibowitz.1

.PHONY: clean
clean:
	rm -rf build
