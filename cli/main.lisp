;;; Entrypoint for the command-line application

(defpackage :leibowitz-cli
  (:use #:cl
        #:leibowitz-core
        #:leibowitz-web)
  (:export #:main))

(in-package :leibowitz-cli)

(defun main ()
  (let ((lib (make-instance 'sqlite-library :db-path #P"leibowitz_main.db")))
    (format T "Created library ~S :3~%Bye for now!~%" lib)))
