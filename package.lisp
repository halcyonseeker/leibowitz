;;; Top level package to make interacting from Lisp nicer

(defpackage :leibowitz
  (:use #:cl
        #:leibowitz.util
        #:leibowitz.core
        #:leibowitz.web
        #:leibowitz.cli))

(in-package :leibowitz)
