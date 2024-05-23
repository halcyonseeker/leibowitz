;;; Some conditions I don't think belong in the core

(in-package :leibowitz.cli)

(define-condition path-is-directory (friendly-error)
  ((path :initarg :path))
  (:report (lambda (c s)
             (with-slots (path) c
               (format s "~A is a directory " path)))))

(define-condition no-such-subcommand (friendly-error)
  ((subcmd :initarg :subcmd))
  (:report (lambda (c s)
             (with-slots (subcmd) c
               (format s "No such subcommand: ~A" subcmd)))))
