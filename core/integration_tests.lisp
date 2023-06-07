;; integration_tests.lisp — Uniformly test every library backend.

(defpackage :leibowitz-core/tests
  (:use #:cl #:leibowitz-core #:parachute))

(in-package :leibowitz-core/tests)

(defmacro define-library-test (name (library &rest tmpfiles) &body body)
  (let ((path (gensym)))
    `(progn
       (define-test ,name :time-limit 1)
       (define-test ,(read-from-string (format NIL "sqlite-library-~A" name))
         :parent ,name
         (let* ((,path (uiop:tmpize-pathname #p"/tmp/leibowitz_core_sqlite_test"))
                (,library (make-instance 'sqlite-library :db-path ,path))
                ,@(loop for var in tmpfiles
                        collect `(,var (uiop:tmpize-pathname
                                        #P"/tmp/leibowitz_core_test_tmpfile"))))
           (unwind-protect (progn ,@body)
             (sqlite:disconnect (slot-value ,library 'leibowitz-core::handle))
             ,@(loop for var in tmpfiles
                     collect `(delete-file ,var))
             (delete-file ,path)))))))

(defmacro with-tmp-files ((&rest tmpfiles) &body body)
  `(let (,@(loop for var in tmpfiles
                 collect `(,var (uiop:tmpize-pathname
                                 #P"/tmp/leibowitz_core_test_tmpfile"))))
     (unwind-protect (progn ,@body)
       ,@(loop for var in tmpfiles
               collect `(delete-file ,var)))))

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
;; Lower-level datum and library behavior

(define-test datum-is-reinitialized-by-mime-type-major-part-only
  (with-tmp-files (path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "hi :3~%"))
    (let ((d (make-instance 'datum :id path)))
      (of-type datum-text d)
      (is #'equal "text/plain" (datum-kind d))
      (is #'equal (format NIL "hi :3~%") (datum-terms d)))))

(define-test datum-is-reinitialized-by-mime-type-full
  (with-tmp-files (path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "<!DOCTYPE html>~%<html><head></head><body>hi :^3</body></html>"))
    (let ((d (make-instance 'datum :id path)))
      (of-type datum-text/html d)
      (is #'equal "text/html" (datum-kind d))
      (is #'equal "hi :^3" (datum-terms d)))))

(define-test datum-is-reinitialized-by-url-scheme-in-id
  (let ((d (make-instance 'datum :id "https://thepiratebay.org")))
    (of-type datum-link/web d)
    (is #'equal "link/web" (datum-kind d))
    (is #'equal "https://thepiratebay.org" (datum-terms d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections API tests

(define-library-test library-get-datum-collection-works-for-homedir (l)
  (labels ((homedir ()
             (find-if (lambda (elem) (eql (type-of elem) 'collection-homedir))
                      (library-collections l))))
    (is #'eq (homedir) (library-get-datum-collection l (user-homedir-pathname)))
    (is #'eq (homedir) (library-get-datum-collection
                        l (merge-pathnames (user-homedir-pathname) "sub")))
    (isnt #'eq (homedir) (library-get-datum-collection l "/hopefully/not/your/~"))))

(define-library-test library-get-datum-collection-works-for-link/web (l)
  (labels ((web ()
             (find-if (lambda (elem) (eql (type-of elem) 'collection-link/web))
                      (library-collections l))))
    (is #'eq (web) (library-get-datum-collection l "https://dreamwidth.org"))
    (is #'eq (web) (library-get-datum-collection l "http://alt.suicide.holiday"))
    (isnt #'eq (web) (library-get-datum-collection l "ftp://yourmomsnudes.zip"))
    (isnt #'eq (web) (library-get-datum-collection l ".sbclrc"))
    (isnt #'eq (web) (library-get-datum-collection l #p".sbclrc"))))

;; homedir-specific methods

;; FIXME: here go a bunch of regex tests for gitignore-like
;; knowability

;; FIXME: other stuff with the root set to a subdirectory of /tmp.

;; link-specific methods

;; FIXME: here go a bunch of tests for downloading URLs and making
;; sure they're indexed as the right kind of datum

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

(define-library-test insert-and-update-datum (l path)
  (let ((d (add-datum l (make-instance 'datum :id path))))
    (setf (datum-terms d) "hi")
    (is #'equal "hi" (datum-terms (add-datum l d)))))

(define-library-test datum-also-reinitialized-when-reading-from-db (l path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (format s "hi :3~%"))
  (add-datum l (make-instance 'datum :id path))
  (let ((d (get-datum l path)))
    (of-type datum-text d)
    (is #'equal "text/plain" (datum-kind d))
    (is #'equal (format NIL "hi :3~%") (datum-terms d))))

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

(define-library-test duplicate-tag-junctions-are-impossible (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum-tags l d '("tag"))
    (add-datum-tags l d '("tag"))
    (is #'= 1 (length (get-datum-tags l d)))))

;;; Tag Predicates

(define-library-test add-predicate-to-a-tag (l)
  (let ((t1 (make-instance 'tag :name "History"))
        (t2 (make-instance 'tag :name "Sassanian Empire")))
    (add-tag l t1)
    (add-tag l t2)
    (false (get-tag-predicates l "History"))
    (false (get-tag-predicands l "Sassanian Empire"))
    (add-tag-predicate l t1 t2)
    (let ((predicates (get-tag-predicates l "History")))
      (is #'= 1 (length predicates))
      (is #'equal "Sassanian Empire" (tag-name (car predicates))))
    (let ((predicands (get-tag-predicands l "Sassanian Empire")))
      (is #'= 1 (length predicands))
      (is #'equal "History" (tag-name (car predicands))))))

(define-library-test add-tag-predicate-implicitly-creates-nonexistent-tags (l)
  (add-tag-predicate l "Zoroastrianism" "People of The Book")
  (is #'equal "People of The Book" (tag-name (car (get-tag-predicates l "Zoroastrianism"))))
  (is #'equal "Zoroastrianism" (tag-name (car (get-tag-predicands l "People of The Book")))))

(define-library-test add-tag-predicate-should-not-create-duplicate-tags (l)
  (add-tag-predicate l "a" "b")
  (add-tag-predicate l "a" "b")
  (is #'= 1 (length (get-tag-predicates l "a")))
  (is #'= 1 (length (get-tag-predicands l "b"))))

(define-library-test add-and-remove-tag-predicate (l)
  (add-tag-predicate l "Marguerite Porete" "Christian Mystics")
  (add-tag-predicate l "Meister Eckhart" "Christian Mystics")
  (is #'= 2 (length (get-tag-predicands l "Christian Mystics")))
  (del-tag-predicate l "Meister Eckhart" "Christian Mystics")
  (true (get-tag l "Meister Eckhart"))
  (is #'= 1 (length (get-tag-predicands l "Christian Mystics")))
  (false (get-tag-predicates l "Meister Eckhart")))

(define-library-test add-datum-tags-cascades-down-predicate-tree (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (add-tag-predicate l "Ibn Rushd" "Islamic Rationalist Philosophers")
    (add-datum-tags l d '("Ibn Rushd"))
    (is #'equal
        '("Ibn Rushd" "Islamic Rationalist Philosophers")
        (sort (loop for tag in (get-datum-tags l d) collect (tag-name tag))
              #'string<))))

(define-library-test del-datum-tags-doesnt-cascade-down-predicate-tree-by-default (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (add-tag-predicate l "Ibn Rushd" "Islamic Rationalist Philosophers")
    (add-datum-tags l d '("Ibn Rushd"))
    (del-datum-tags l d '("Ibn Rushd"))
    (is #'= 1 (length (get-datum-tags l d)))))

(define-library-test del-datum-tags-may-cascade-down-predicate-tree (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (add-tag-predicate l "Ibn Rushd" "Islamic Rationalist Philosophers")
    (add-datum-tags l d '("Ibn Rushd"))
    (del-datum-tags l d '("Ibn Rushd") :cascade T)
    (false (get-datum-tags l d))))

(define-library-test cycles-in-tag-hierarchy-are-detected (l path)
  ;; This will timeout if it fails
  (let ((d (make-instance 'datum :id path)))
    (add-tag-predicate l "first" "second")
    (add-tag-predicate l "second" "third")
    (add-tag-predicate l "third" "first")
    (add-datum-tags l d '("first"))))

(define-library-test del-tag-removes-predicate-associations (l)
  (add-tag-predicate l "science fiction" "fiction")
  (add-tag-predicate l "fiction" "entertainment")
  (del-tag l "fiction")
  (false (get-tag-predicates l "science fiction"))
  (false (get-tag-predicands l "entertainment")))

;;; Searching and Listing

(define-library-test search-for-string (l path)
  (let ((d (add-datum l (make-instance 'datum :id path))))
    ;; This will fial if the file isn't in /tmp
    (is #'equal (datum-id d) (datum-id (car (query l "tmp" NIL NIL))))))

;; FIXME: Write tests for sorting and listing

