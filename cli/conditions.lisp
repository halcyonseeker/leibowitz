;;; Some conditions I don't think belong in the core

(in-package :leibowitz.cli)

(define-condition path-is-directory (friendly-error)
  ((path :initarg :path))
  (:report (lambda (c s)
             (with-slots (path) c
               (format s "~A is a directory " path)))))
