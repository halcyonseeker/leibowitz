;;; Entrypoint for the command-line application

(defpackage :leibowitz
  (:use #:cl
        #:leibowitz-core)
  (:export #:main))

(in-package :leibowitz)

(defun main ()
  (let ((lib (make-instance 'sqlite-library :db-path #P"leibowitz_main.db")))
    (format T "Created library ~S :3~%Bye for now!~%" lib)))
