;;; Top level package to make interacting from Lisp nicer

(defpackage :leibowitz
  (:use #:cl
        #:leibowitz-core
        #:leibowitz-web
        #:leibowitz-cli))

(in-package :leibowitz)
