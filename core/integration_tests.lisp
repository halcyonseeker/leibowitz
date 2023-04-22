;; integration_tests.lisp — Uniformly test every library backend.

(defpackage :leibowitz-core/tests
  (:use #:cl #:leibowitz-core #:parachute))

(in-package :leibowitz-core/tests)

(defmacro define-library-test (name (library &rest tmpfiles) &body body)
  (let ((path (gensym)))
    `(progn
       (define-test ,name)
       (define-test ,(read-from-string (format NIL "sqlite-library-~A" name))
         :parent ,name
         (let* ((,path (uiop:tmpize-pathname #p"/tmp/leibowitz_core_sqlite_test"))
                (,library (make-instance 'sqlite-library :db-path ,path))
                ,@(loop for var in tmpfiles
                        collect `(,var (uiop:tmpize-pathname
                                        #P"/tmp/leibowitz_core_test_tmpfile"))))
           (unwind-protect (progn ,@body)
             ,@(loop for var in tmpfiles
                     collect `(delete-file ,var))
             (delete-file ,path)))))))

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
;; Lower-level datum tests

;; (define-test initialize-datum)

;; (define-test reinitialize-datum-class-by-mime-type)

;; (define-test reinitialize-datum-class-by-directory)

;; (define-test reinitialize-datum-class-not-backed-by-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic API tests

(define-library-test insert-datum (l path)
  (let ((d (make-instance 'datum :id path)))
    (is #'eq d (add-datum l d))))

(define-library-test retrieve-nonexistent-datum (l)
  (false (get-datum l "some nonexistent id"))
  (false (get-datum l #P"/this/time/a/pathname")))

(define-library-test delete-nonexistent-datum (l)
  (del-datum l "some datum id"))

;; FIXME: writing some tests for leibowitz-core::%datum-equal would be worthwhile

(define-library-test insert-and-retrieve-datum (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (is #'leibowitz-core::%datum-equal d (get-datum l (datum-id d)))))

(define-library-test insert-and-delete-datum (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (del-datum l d)
    (false (get-datum l (datum-id d)))))

;;; Tagging

(define-library-test add-single-tag-to-datum-then-remove (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (add-datum-tags l d '("tag"))
    (let ((tag (car (get-datum-tags l d))))
      (is #'equal "tag" (tag-name tag))
      (is #'= 1 (tag-count tag)))
    (is #'equal (datum-id d) (datum-id (car (get-tag-data l "tag"))))
    (del-datum-tags l d '("tag"))
    (false (get-datum-tags l d))
    (false (get-tag-data l "tag"))
    (is #'leibowitz-core::%datum-equal d (get-datum l (datum-id d)))))

(define-library-test removing-datum-removes-its-tags (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (add-datum-tags l d '("some" "tags"))
    (del-datum l d)
    (false (get-tag l "some"))
    (false (get-tag l "tags"))))

(define-library-test decrement-count-when-data-removed-from-tag-with-other-data (l path1 path2)
  (let ((d1 (make-instance 'datum :id path1))
        (d2 (make-instance 'datum :id path2)))
    (add-datum l d1)
    (add-datum l d2)
    (add-datum-tags l d1 '("tag"))
    (add-datum-tags l d2 '("tag"))
    (is #'= 2 (length (get-tag-data l "tag")))
    (is #'= 2 (tag-count (get-tag l "tag")))
    (del-datum l d1)
    (is #'= 1 (length (get-tag-data l "tag")))
    (is #'= 1 (tag-count (get-tag l "tag")))))

(define-library-test removing-tag-removes-all-its-associations (l path1 path2)
  (let ((d1 (make-instance 'datum :id path1))
        (d2 (make-instance 'datum :id path2)))
    (add-datum l d1)
    (add-datum l d2)
    (add-datum-tags l d1 '("tag"))
    (add-datum-tags l d2 '("tag"))
    (del-tag l "tag")
    (false (get-tag-data l "tag"))
    (false (get-datum-tags l d1))
    (false (get-datum-tags l d2))))

(define-library-test create-empty-tag-with-label-and-not-orphaned-when-del-datum (l path)
  (let ((tag (make-instance 'tag :name "tag" :label "hi")))
    (add-tag l tag)
    (is #'= 0 (tag-count (get-tag l "tag")))
    (let ((d (make-instance 'datum :id path)))
      (add-datum-tags l d '("tag"))
      (is #'= 1 (tag-count (get-tag l "tag")))
      (del-datum l d)
      (is #'= 0 (tag-count (get-tag l "tag"))))))

;;; Tag Predicates
