
(defpackage :leibowitz.build
  (:use #:cl)
  (:export #:discover-systems
           #:build
           #:test))
(in-package :leibowitz.build)

(defun discover-systems (&optional (dir *default-pathname-defaults*))
  (when (uiop:directory-exists-p dir)
    (when (member "asd" (mapcar #'pathname-type (uiop:directory-files dir))
                  :test #'equal)
      (push dir asdf:*central-registry*))
    (mapcar #'discover-systems (uiop:subdirectories dir))))

(defun call-quickload (system)
  "Prevent the Lisp reader from yelling about package QL not existing."
  (funcall (find-symbol "QUICKLOAD" :ql) system))

(defun build ()
  (format T "Using Quicklisp? ~S~%" (uiop:getenv "WITH_QUICKLISP"))
  (if (uiop:getenv "WITH_QUICKLISP")
      (call-quickload :leibowitz)
      (discover-systems #P"build/dependencies/"))
  (asdf:make :leibowitz)
  (uiop:quit))

(defun test ()
  (format T "Using Quicklisp? ~S~%" (uiop:getenv "WITH_QUICKLISP"))
  (if (uiop:getenv "WITH_QUICKLISP")
      (call-quickload :leibowitz/tests)
      (discover-systems #P"build/dependencies/"))
  (asdf:test-system :leibowitz)
  (uiop:quit))
