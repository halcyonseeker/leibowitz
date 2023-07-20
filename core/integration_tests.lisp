;; integration_tests.lisp — Uniformly test every library backend.

(defpackage :leibowitz-core/tests
  (:use #:cl #:leibowitz-core #:parachute))

(in-package :leibowitz-core/tests)

(defmacro define-library-test (name (library &rest tmpfiles) &body body)
  (let ((path (gensym))
        (home (gensym)))
    `(progn
       (define-test ,name :time-limit 2)
       (define-test ,(read-from-string (format NIL "sqlite-library-~A" name))
         :parent ,name
         (let* ((,home (ensure-directories-exist
                        (pathname
                         (format NIL "/tmp/leibowitz_core_test_home-tmp~36R/"
                                 (random (expt 36 8))))))
                (,path (uiop:tmpize-pathname #p"/tmp/leibowitz_core_sqlite_test"))
                (,library (make-instance 'sqlite-library :db-path ,path :homedir ,home))
                ,@(loop for var in tmpfiles
                        collect `(,var (uiop:tmpize-pathname
                                        (merge-pathnames ,home #P"testfile")))))
           (unwind-protect (progn ,@body)
             (sqlite:disconnect (slot-value ,library 'leibowitz-core::handle))
             ,@(loop for var in tmpfiles
                     collect `(delete-file ,var))
             (uiop:delete-directory-tree ,home :validate T)
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

(define-test library-get-datum-collection-works-for-homedir
  (let ((l (make-instance 'library)))
    (labels ((homedir ()
               (find-if (lambda (elem) (eql (type-of elem) 'collection-homedir))
                        (library-collections l))))
      (is #'eq (homedir) (library-get-datum-collection l (user-homedir-pathname)))
      (is #'eq (homedir) (library-get-datum-collection
                          l (merge-pathnames (user-homedir-pathname) "sub")))
      (isnt #'eq (homedir) (library-get-datum-collection l "/hopefully/not/your/~")))))

(define-test library-get-datum-collection-works-for-link/web
  (let ((l (make-instance 'library)))
    (labels ((web ()
               (find-if (lambda (elem) (eql (type-of elem) 'collection-link/web))
                        (library-collections l))))
      (is #'eq (web) (library-get-datum-collection l "https://dreamwidth.org"))
      (is #'eq (web) (library-get-datum-collection l "http://alt.suicide.holiday"))
      (isnt #'eq (web) (library-get-datum-collection l "ftp://yourmomsnudes.zip"))
      (isnt #'eq (web) (library-get-datum-collection l ".sbclrc"))
      (isnt #'eq (web) (library-get-datum-collection l #p".sbclrc")))))

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
  (let ((d (make-instance 'datum :id path :collection (library-get-datum-collection l path))))
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

(define-library-test datum-accesses-preserved (l path)
  (let ((d (add-datum l (make-instance 'datum :id path))))
    (is #'= 0 (datum-accesses d))
    (incf (datum-accesses d))
    (add-datum l d)
    (is #'= 1 (datum-accesses (get-datum l path)))))

(define-library-test get-datum-quantity (l p1 p2 p3)
  (add-datum l (make-instance 'datum :id p1))
  (add-datum l (make-instance 'datum :id p2))
  (add-datum l (make-instance 'datum :id p3))
  (is #'= 3 (library-data-quantity l)))

;;; High-level indexer method

(define-library-test index-single-file (l path)
  (let ((indexed (index l path)))
    (true indexed)
    (is #'leibowitz-core::%datum-equal (car indexed) (get-datum l path))))

(define-library-test index-url (l)
  (let ((indexed (index l "https://nyaa.si")))
    (true indexed)
    (is #'leibowitz-core::%datum-equal
        (car indexed) (get-datum l "https://nyaa.si"))))

(define-library-test index-flat-directory (l p1 p2 p3 p4 p5)
  (let ((home (collection-homedir-root
               (find-if (lambda (c)
                          (eql (type-of c) 'collection-homedir))
                        (library-collections l)))))
    (index l home)
    (is #'equal (namestring p1) (datum-id (get-datum l p1)))
    (is #'equal (namestring p2) (datum-id (get-datum l p2)))
    (is #'equal (namestring p3) (datum-id (get-datum l p3)))
    (is #'equal (namestring p4) (datum-id (get-datum l p4)))
    (is #'equal (namestring p5) (datum-id (get-datum l p5)))))

(define-library-test index-directory-tree (l p1)
  (let ((home (collection-homedir-root
                           (find-if (lambda (c)
                                      (eql (type-of c) 'collection-homedir))
                                    (library-collections l)))))
    (labels ((touchsub (name)
               (let ((sub (ensure-directories-exist
                           (merge-pathnames home #P"sub/"))))
                 (uiop:tmpize-pathname (merge-pathnames sub name)))))
      (let ((p2 (touchsub #P"p2"))
            (p3 (touchsub #P"p3"))
            (p4 (touchsub #P"p4"))
            (p5 (touchsub #P"p5")))
        (index l home)
        (is #'equal (namestring p1) (datum-id (get-datum l p1)))
        (is #'equal (namestring p2) (datum-id (get-datum l p2)))
        (is #'equal (namestring p3) (datum-id (get-datum l p3)))
        (is #'equal (namestring p4) (datum-id (get-datum l p4)))
        (is #'equal (namestring p5) (datum-id (get-datum l p5)))))))

;;; Tagging

(define-library-test add-single-tag-to-datum-then-remove (l path)
  (let ((d (make-instance 'datum :id path :collection (library-get-datum-collection l path))))
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

;; FIXME: right now there is no way to update the tag label after
;; creation; add-tag doesn't do anything if there's already a tag of
;; the same name in order to avoid clobbering the count, this would
;; probably best be solved with some kind of trigger to make it
;; impossible for the user to modify the count manually.  God I this
;; API is so poorly defined 😩
;; (define-library-test update-tag-label (l path)
;;   (let ((d (make-instance 'datum :id path :collection (library-get-datum-collection l path))))
;;     (add-datum l d)
;;     (add-datum-tags l d '("tag"))
;;     (add-tag l (make-instance 'tag :name "tag" :label "description"))
;;     (is #'equal "description" (tag-label (get-tag l "tag")))
;;     (is #'equal 1 (tag-count (get-tag l "tag")))))

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
    (is #'equal (datum-id d) (datum-id (car (query l "tmp"))))))

(define-library-test list-data-test-sort-and-direction (l p1 p2)
  (add-datum l (make-instance 'datum :id p1))
  (sleep 1) ;; timestamps have a granularity of 1 second smh
  (with-open-file (s p2 :direction :output :if-exists :supersede)
    (format s "hi :3~%"))
  (add-datum l (make-instance 'datum :id p2))
  ;; most recently modified first
  (destructuring-bind (d1 d2)
      (list-data l :sort-by :modified :direction :descending)
    (true (>= (datum-modified d1) (datum-modified d2))))
  ;; least recently modified first
  (destructuring-bind (d1 d2)
      (list-data l :sort-by :modified :direction :ascending)
    (true (<= (datum-modified d1) (datum-modified d2))))
  ;; most recently created first
  (destructuring-bind (d1 d2)
      (list-data l :sort-by :birth :direction :descending)
    (true (>= (datum-birth d1) (datum-birth d2))))
  ;; least recently created first
  (destructuring-bind (d1 d2)
      (list-data l :sort-by :birth :direction :ascending)
    (true (<= (datum-birth d1) (datum-birth d2)))))

(define-library-test list-data-can-paginate-with-offset-limit (l p1 p2 p3 p4 p5 p6)
  (loop for p in (list p1 p2 p3 p4 p5 p6)
        do (add-datum l (make-instance 'datum :id p)))
  (is #'= 6 (length (list-data l)))
  (is #'= 3 (length (list-data l :offset 0 :limit 3)))
  (is #'= 1 (length (list-data l :offset 5 :limit 1)))
  (is #'= 0 (length (list-data l :offset 0 :limit 0)))
  (fail (list-data l :limit 2))
  (fail (list-data l :offset 9))
  (is #'equal (datum-id (get-datum l p1))
      (datum-id (car (list-data l :offset 0 :limit 1))))
  (is #'equal (datum-id (get-datum l p6))
      (datum-id (car (list-data l :offset 5 :limit 1)))))
