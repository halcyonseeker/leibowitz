
all: build

build: leibowitz.asd *.lisp core/*.lisp
	mkdir -p build
	sbcl --load leibowitz.asd \
	     --eval '(progn (ql:quickload :leibowitz) (asdf:make :leibowitz) (quit))'

.PHONY: clean
clean:
	rm -rf build
