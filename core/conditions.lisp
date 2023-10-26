;;; Define conditions used by the core library

(in-package :leibowitz.core)

;;; FIXME: Use me in practice!
(define-condition friendly-error (error)
  ((msg :initarg :msg))
  (:report (lambda (c s)
             (with-slots (msg) c
               (format s msg))))
  (:documentation "Superclass of errors that are expected to occur, typically as a result
of user error.  All errors that don't inherit from this should be
assumed to be the result of bugs and shown to the user in the loudest,
most technically useful way possible."))

(define-condition datum-not-indexed (friendly-error)
  ((id :initarg :id)
   (lib :initarg :lib))
  (:report (lambda (c s)
             (with-slots (id lib) c
               (format s "Datum with id ~S not present in ~S~%" id lib)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unfriendly errors

(define-condition no-applicable-collection (error)
  ((id :initarg :id))
  (:report (lambda (c s)
             (with-slots (id) c (format s "No collection found for datum ~S" id)))))
