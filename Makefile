LISP_SOURCES_REGEX = *.asd *.lisp */*.asd */*.lisp */*/*.lisp

all: build

build: $(LISP_SOURCES_REGEX)
	mkdir -p build
	sbcl --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz) (asdf:make :leibowitz) (quit))'

test: $(LISP_SOURCES_REGEX)
	sbcl --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz) (asdf:test-system :leibowitz-core) (quit))'

.PHONY: clean
clean:
	rm -rf build
