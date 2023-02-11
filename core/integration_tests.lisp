;; integration_tests.lisp — Uniformly test every library backend.

(defpackage :leibowitz-core/tests
  (:use #:cl #:leibowitz-core #:parachute))

(in-package :leibowitz-core/tests)

(defmacro define-library-test (name &body body)
  `(progn
     (define-test ,name)
     (define-test ,(read-from-string (format NIL "sqlite-library-~A" name))
       :parent ,name
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend-specific tests

(define-test create-sqlite-library
  (let* ((path (uiop:tmpize-pathname #p"/tmp/leibowitz_core_testing_sqlite_db"))
         (lbry (make-instance 'sqlite-library :db-path path)))
    (unwind-protect
         (progn
           (false (sqlite-rows lbry "select * from tags"))
           (false (sqlite-rows lbry "select * from data"))
           (false (sqlite-rows lbry "select * from tag_datum_junctions"))
           (false (sqlite-rows lbry "select * from tag_predicates")))
      (delete-file path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic API tests

(define-library-test insert-and-retrieve-datum
  (true T)
  (skip "will fail"
    (true NIL)))
