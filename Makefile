LISP_SOURCES_REGEX = *.asd *.lisp */*.asd */*.lisp */*/*.lisp

all: build

build: $(LISP_SOURCES_REGEX)
	mkdir -p build
	sbcl --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz) (asdf:make :leibowitz) (quit))'

test: $(LISP_SOURCES_REGEX)
	sbcl --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz) (asdf:test-system :leibowitz-core) (quit))'

completions: build
	build/leibowitz --zsh-completions > build/_leibowitz
	@ # I install to the ~/.local prefix for testing but zsh
	@ # doesn't search any of the home directory by default.  This
	@ # line loads the completions.
	@ echo Now run this in order to load the completions:
	@ echo 'export fpath=(~/.local/share/zsh/site-functions $$fpath); autoload -U compinit; compinit'

.PHONY: clean
clean:
	rm -rf build
