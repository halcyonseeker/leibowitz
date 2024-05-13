;; integration_tests.lisp — Uniformly test every library backend.

(in-package :leibowitz/tests)

(define-test core)

(defmacro define-library-test (name (library &rest tmpfiles) &body body)
  (let ((path (gensym))
        (home (gensym)))
    `(progn
       (define-test ,name :time-limit 2 :parent core)
       (define-test ,(read-from-string (format NIL "sqlite-library-~A" name))
         :parent ,name
         (let* ((,home (ensure-directories-exist
                        (pathname
                         (format NIL "/tmp/leibowitz_core_test_home-tmp~36R/"
                                 (random (expt 36 8))))))
                (,path (uiop:tmpize-pathname #p"/tmp/leibowitz_core_sqlite_test"))
                (,library (make-instance 'sqlite-library :db-path ,path :homedir ,home
                                                         :thumbnail-cache-dir ,home))
                ,@(loop for var in tmpfiles
                        collect `(,var (uiop:tmpize-pathname
                                        (merge-pathnames ,home #P"testfile")))))
           (unwind-protect (progn ,@body)
             (sqlite:disconnect (slot-value ,library 'leibowitz.core::handle))
             ,@(loop for var in tmpfiles
                     collect `(ignore-errors (delete-file ,var)))
             (uiop:delete-directory-tree ,home :validate T)
             (ignore-errors (delete-file ,path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend-specific tests

(define-test create-sqlite-library :parent core
  (let* ((path (uiop:tmpize-pathname #p"/tmp/leibowitz_core_testing_sqlite_db"))
         (lbry (make-instance 'sqlite-library :db-path path
                                              :thumbnail-cache-dir "")))
    (unwind-protect
         (progn
           (false (sqlite-rows lbry "select * from tags"))
           (false (sqlite-rows lbry "select * from data"))
           (false (sqlite-rows lbry "select * from tag_datum_junctions"))
           (false (sqlite-rows lbry "select * from tag_predicates")))
      (delete-file path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lower-level datum and library behavior

(define-test datum-is-reinitialized-by-mime-type-major-part-only :parent core
  (with-tmp-files (path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "hi :3~%"))
    (let ((d (make-instance 'datum :id path)))
      (of-type datum-text d)
      (is #'equal "text/plain" (datum-kind d))
      (is #'equal (format NIL "hi :3~%") (datum-terms d)))))

(define-test datum-is-reinitialized-by-mime-type-full :parent core
  (with-tmp-files (path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "<!DOCTYPE html>~%<html><head></head><body>hi :^3</body></html>"))
    (let ((d (make-instance 'datum :id path)))
      (of-type datum-text/html d)
      (is #'equal "text/html" (datum-kind d))
      (is #'equal "hi :^3" (datum-terms d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections API tests

(define-test library-get-datum-collection-works-for-homedir :parent core
  (let ((l (make-instance 'library :thumbnail-cache-dir #P"/tmp/")))
    (labels ((homedir ()
               (find-if (lambda (elem) (eql (type-of elem) 'collection-homedir))
                        (library-collections l))))
      (is #'eq (homedir) (library-get-datum-collection l (user-homedir-pathname)))
      (is #'eq (homedir) (library-get-datum-collection
                          l (merge-pathnames (user-homedir-pathname) "sub")))
      ;; FIXME: this additional test case was added after manual
      ;; testing on FreeBSD failed weirdly as a result of the fact
      ;; that it symlinks /home to /usr/home.  We should write a
      ;; proper test case where homedir's root slot has a symlink.
      (is #'eq (homedir) (library-get-datum-collection
                          l (merge-pathnames (truename (user-homedir-pathname)) "sub")))
      (fail (library-get-datum-collection l "/hopefully/not/your/~")
          'no-applicable-collection))))

;; homedir-specific methods

;; FIXME: here go a bunch of regex tests for gitignore-like
;; knowability

;; FIXME: other stuff with the root set to a subdirectory of /tmp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic API tests

(define-library-test data-cannot-be-exotic-file-types (l path)
  (let ((p1 (mktmp #P"/tmp/" :fifo))
        (p2 (mktmp #P"/tmp/" :dir)))
    (unwind-protect
         (progn
           (fail (make-instance 'datum :id p1) 'file-not-regular)
           (fail (make-instance 'datum :id p2) 'file-not-regular))
      (delete-file p1)
      (uiop:delete-empty-directory p2))))

(define-library-test insert-datum (l path)
  (let ((d (make-instance 'datum :id path)))
    (is #'eq d (add-datum l d))))

(define-library-test retrieve-nonexistent-datum (l)
  (false (get-datum l "some nonexistent id"))
  (false (get-datum l #P"/this/time/a/pathname")))

(define-library-test retrieve-nonexistent-datum-error (l)
  (fail (get-datum l "Captain Nemo" :error T) 'datum-not-indexed))

(define-library-test delete-nonexistent-datum (l)
  (del-datum l "some datum id")
  (fail (del-datum l "no" :error T) 'datum-not-indexed))

(define-library-test delete-datum-disk-keyword (l path path2)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (del-datum l path :disk NIL)
    (true (probe-file path))
    (add-datum l d)
    (del-datum l path)
    (false (probe-file path))
    (add-datum l (make-instance 'datum :id path2))
    (delete-file path2)
    (fail (del-datum l path2 :error T) 'datum-is-orphaned)))

;; FIXME: writing some tests for leibowitz.core::%datum-equal would be worthwhile

(define-library-test insert-and-retrieve-datum (l path)
  (let ((d (make-instance 'datum :id path :collection (library-get-datum-collection l path))))
    (add-datum l d)
    (is #'leibowitz.core::%datum-equal d (get-datum l (datum-id d)))))

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
    (is #'leibowitz.core::%datum-equal (car indexed) (get-datum l path))))

(define-library-test index-list-of-files (l p1 p2 p3 p4 p5)
  (let* ((paths (list p1 p2 p3 p4 p5))
         (indexed (index l paths)))
    (is #'= 5 (length indexed))
    (is #'equal (namestring p1) (datum-id (get-datum l p1)))
    (is #'equal (namestring p2) (datum-id (get-datum l p2)))
    (is #'equal (namestring p3) (datum-id (get-datum l p3)))
    (is #'equal (namestring p4) (datum-id (get-datum l p4)))
    (is #'equal (namestring p5) (datum-id (get-datum l p5)))))

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

;; FIXME: add tests for all the new functionality described in the
;; `index' fixme!

;;; Tagging

(define-library-test add-single-tag-to-datum-then-remove (l path)
  (let ((d (make-instance 'datum :id path :collection (library-get-datum-collection l path))))
    (add-datum l d)
    (add-datum-tags l d '("tag"))
    (let ((tag (car (get-datum-tags l d))))
      (is #'equal "tag" (tag-name tag))
      (is #'= 1 (tag-count tag)))
    (is #'equal (datum-id d) (datum-id (car (get-tag-data l "tag"))))
    (is #'= 1 (datum-num-tags l d))
    (del-datum-tags l d '("tag"))
    (is #'= 0 (datum-num-tags l d))
    (false (get-datum-tags l d))
    (false (get-tag-data l "tag"))
    (is #'leibowitz.core::%datum-equal d (get-datum l (datum-id d)))))

(define-library-test replace-datum-tags (l path)
  (let ((d (make-instance 'datum :id path :collection (library-get-datum-collection l path))))
    (add-datum l d)
    (add-datum-tags l d '("tag"))
    (let ((tags (get-datum-tags l d)))
      (is #'equal 1 (length tags))
      (is #'equal "tag" (tag-name (car tags))))
    (add-datum-tags l d '("gat") :replace T)
    (let ((tags (get-datum-tags l d)))
      (is #'equal 1 (length tags))
      (is #'equal "gat" (tag-name (car tags))))))

(define-library-test cannot-add-tags-to-nonexistent-datum (l)
  (fail (add-datum-tags l "no datum with this id" '("asdf"))
        'datum-not-indexed))

(define-library-test update-tag-label (l path)
  (let ((d (make-instance 'datum :id path :collection (library-get-datum-collection l path))))
    (add-datum l d)
    (add-datum-tags l d '("tag"))
    (add-tag l (make-instance 'tag :name "tag" :label "description"))
    (is #'equal "description" (tag-label (get-tag l "tag")))
    (is #'equal 1 (tag-count (get-tag l "tag")))))

(define-library-test removing-datum-removes-its-tags (l path)
  (let ((d (make-instance 'datum :id path)))
    (add-datum l d)
    (add-datum-tags l d '("some" "tags"))
    (del-datum l d)
    (false (get-tag l "some"))
    (fail (get-tag l "tags" :error T) 'no-such-tag)))

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
    (let ((d (add-datum l (make-instance 'datum :id path))))
      (add-datum-tags l d '("tag"))
      (is #'= 1 (tag-count (get-tag l "tag")))
      (del-datum l d)
      (is #'= 0 (tag-count (get-tag l "tag"))))))

(define-library-test duplicate-tag-junctions-are-impossible (l path)
  (let ((d (add-datum l (make-instance 'datum :id path))))
    (add-datum-tags l d '("tag"))
    (add-datum-tags l d '("tag"))
    (is #'= 1 (length (get-datum-tags l d)))))

(define-library-test move-tag-simple-case (l p1 p2)
  (index l p1)
  (index l p2)
  (add-datum-tags l p1 '("some tag"))
  (add-datum-tags l p2 '("some tag"))
  (add-tag-predicate l "some tag" "also add" :retroactive T)
  (move-tag l "some tag" "new")
  (false (get-tag l "some tag"))
  (false (get-tag-data l "some tag"))
  (false (get-tag-predicates l "some tag"))
  (true (get-tag l "new"))
  (is #'= 2 (length (get-tag-data l "new")))
  (is #'= 2 (length (get-datum-tags l p1)))
  (is #'= 2 (length (get-datum-tags l p2)))
  (is #'equal "also add" (tag-name (car (get-tag-predicates l "new")))))

(define-library-test move-tag-no-old-tag (l)
  (fail (move-tag l "no" "where") 'no-such-tag))

(define-library-test move-tag-theyre-the-same (l)
  (fail (move-tag l "here" "here") 'cannot-mv-or-cp-to-itself))

(define-library-test move-tag-cannot-merge-and-overwrite (l)
  (fail (move-tag l "here" "there" :merge T :overwrite T)))

(define-library-test move-tag-new-exists-default (l p)
  (add-datum-tags l (car (index l p)) '("src" "dst"))
  (fail (move-tag l "src" "dst") 'tag-already-exists))

;; FIXME: We need a policy for handling predicand tags too!
(define-library-test move-tag-new-exists-overwrite (l p1 p2)
  (add-datum-tags l (car (index l p1)) '("src" "dst"))
  (add-datum-tags l (car (index l p2)) '("src"))
  (add-tag-predicate l "dst" "destination" :retroactive T)
  (add-tag-predicate l "src" "source" :retroactive T)
  (move-tag l "src" "dst" :overwrite T)
  (false (get-tag l "src"))
  (true (get-tag l "dst"))
  (true (get-tag l "source"))
  (true (get-tag l "destination"))
  (is #'= 3 (length (list-tags l)))
  (is #'= 3 (length (get-datum-tags l p1)))
  (is #'= 2 (length (get-datum-tags l p2)))
  (is #'= 1 (length (get-tag-predicates l "dst")))
  (is #'equal "source" (tag-name (car (get-tag-predicates l "dst")))))

;; FIXME: We need a policy for handling predicand tags too!
(define-library-test move-tag-new-exists-merge (l p1 p2)
  (add-datum-tags l (car (index l p1)) '("src" "dst"))
  (add-datum-tags l (car (index l p2)) '("src"))
  (add-tag-predicate l "dst" "destination" :retroactive T)
  (add-tag-predicate l "src" "source" :retroactive T)
  (move-tag l "src" "dst" :merge T)
  (false (get-tag l "src"))
  (true (get-tag l "dst"))
  (true (get-tag l "source"))
  (true (get-tag l "destination"))
  (is #'= 3 (length (list-tags l)))
  (is #'= 3 (length (get-datum-tags l p1)))
  (is #'= 3 (length (get-datum-tags l p2)))
  (is #'= 2 (length (get-tag-predicates l "dst"))))

(define-library-test copy-tag-simple-case (l p1 p2)
  (index l p1)
  (index l p2)
  (let ((tag (make-instance 'tag :name "tag" :label "about me")))
    (add-datum-tags l p1 (list tag))
    (add-datum-tags l p2 (list tag)))
  (add-tag-predicate l "tag" "me too" :retroactive T)
  (copy-tag l "tag" "new")
  (true (get-tag l "tag"))
  (true (get-tag l "new"))
  ;; FIXME: There's a bug in add-datum-tags that causes labels to be
  ;; discarded!
  ;; (is #'equal "about me" (tag-label (get-tag l "tag")))
  ;; (is #'equal "about me" (tag-label (get-tag l "new")))
  (is #'equal "me too" (tag-name (car (get-tag-predicates l "tag"))))
  (is #'equal "me too" (tag-name (car (get-tag-predicates l "new"))))
  (is #'= 2 (length (get-tag-predicands l "me too")))
  (is #'= 1 (length (get-tag-predicates l "tag")))
  (is #'= 1 (length (get-tag-predicates l "new")))
  (is #'= 2 (length (get-tag-data l "new")))
  (is #'= 2 (length (get-tag-data l "tag")))
  (is #'= 2 (length (get-tag-data l "me too")))
  (is #'= 3 (length (get-datum-tags l p1)))
  (is #'= 3 (length (get-datum-tags l p2))))

(define-library-test copy-tag-no-old-tag (l)
  (fail (copy-tag l "no" "where") 'no-such-tag))

(define-library-test copy-tag-theyre-the-same (l)
  (fail (copy-tag l "here" "here") 'cannot-mv-or-cp-to-itself))

(define-library-test copy-tag-cannot-merge-and-overwrite (l)
  (fail (copy-tag l "here" "there" :merge T :overwrite T)))

(define-library-test copy-tag-new-exists-default (l p)
  (add-datum-tags l (car (index l p)) '("src" "dst"))
  (fail (copy-tag l "src" "dst") 'tag-already-exists))

;; FIXME: We need a policy for handling predicand tags too!
(define-library-test copy-tag-new-exists-overwrite (l p1 p2)
  (index l p1)
  (index l p2)
  (let ((src (make-instance 'tag :name "src" :label "there'll be two of me :3"))
        (dst (make-instance 'tag :name "dst" :label "original dst label")))
    (add-datum-tags l p1 (list src))
    (add-datum-tags l p2 (list src dst)))
  (add-tag-predicate l "src" "source" :retroactive T)
  (add-tag-predicate l "dst" "destination" :retroactive T)
  (copy-tag l "src" "dst" :overwrite T)
  (true (get-tag l "src"))
  (true (get-tag l "dst"))
  ;; FIXME: I believe these are being discarded by the same bug
  ;; (is #'equal "there'll be two of me :3" (tag-label (get-tag l "src")))
  ;; (is #'equal "there'll be two of me :3" (tag-label (get-tag l "dst")))
  (is #'= 1 (length (get-tag-predicates l "dst")))
  (is #'= 1 (length (get-tag-predicates l "src")))
  (is #'= 2 (length (get-tag-predicands l "source")))
  (is #'= 0 (length (get-tag-predicands l "destination")))
  (is #'= 3 (length (get-datum-tags l p1)))
  (is #'= 4 (length (get-datum-tags l p2))))

;; ;; FIXME: We need a policy for handling predicand tags too!
(define-library-test copy-tag-new-exists-merge (l p1 p2)
  (index l p1)
  (index l p2)
  (let ((src (make-instance 'tag :name "src" :label "there'll be two of me :3"))
        (dst (make-instance 'tag :name "dst" :label "original dst label")))
    (add-datum-tags l p1 (list src))
    (add-datum-tags l p2 (list dst)))
  (add-tag-predicate l "src" "source" :retroactive T)
  (add-tag-predicate l "dst" "destination" :retroactive T)
  (copy-tag l "src" "dst" :merge T)
  (true (get-tag l "src"))
  (true (get-tag l "dst"))
  ;; FIXME: I believe these are being discarded by the aforementioned bug
  ;; (is #'equal "there'll be two of me :3" (tag-label (get-tag l "src")))
  ;; (is #'equal "there'll be two of me :3" (tag-label (get-tag l "dst")))
  (is #'= 2 (length (get-tag-predicates l "dst")))
  (is #'= 1 (length (get-tag-predicates l "src")))
  (is #'= 2 (length (get-tag-predicands l "source")))
  (is #'= 1 (length (get-tag-predicands l "destination")))
  (is #'= 4 (length (get-datum-tags l p1)))
  (is #'= 3 (length (get-datum-tags l p2))))

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

(define-library-test add-predicate-to-a-tag-and-add-it-to-data-retroactively (l p)
  (let ((d (add-datum l (make-instance 'datum :id p))))
    (add-datum-tags l d '("Ganymede"))
    (add-tag-predicate l "Ganymede" "Galilean Moons" :retroactive NIL)
    (let ((tags (get-datum-tags l d)))
      (is #'= 1 (length tags)))
    (add-tag-predicate l "Ganymede" "Galilean Moons")
    (let ((tags (get-datum-tags l d)))
      (is #'= 2 (length tags)))))

(define-library-test replace-tag-predicates (l p)
  (let ((d (add-datum l (make-instance 'datum :id p))))
    (add-datum-tags l d '("Ganymede"))
    (add-tag-predicate l "Ganymede" '("Galilean Moons" "Jovian System" "oops"))
    (let ((tags (get-datum-tags l d)))
      (is #'= 4 (length tags)))
    (let ((subtags (get-tag-predicates l "Ganymede")))
      (is #'= 3 (length subtags)))
    (add-tag-predicate l "Ganymede" '("Galilean Moons" "Jovian System") :replace T)
    ;; FIXME: here we see that the :replace behavior doesn't work
    ;; retroactively, we'll need to give del-tag-predicate an option
    ;; for that too and test that behavior here
    (let ((tags (get-datum-tags l d)))
      (is #'= 4 (length tags)))
    (let ((subtags (get-tag-predicates l "Ganymede")))
      (format T "                    SUBTAGS: (2) ~S~%" subtags)
      (is #'= 2 (length subtags)))))

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
  (let ((d (add-datum l (make-instance 'datum :id path))))
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

(define-library-test query-can-paginate-with-offset-limit (l p1 p2 p3 p4 p5 p6)
  (loop for p in (list p1 p2 p3 p4 p5 p6)
        do (add-datum l (make-instance 'datum :id p)))
  (is #'= 6 (length (query l "tmp")))
  (is #'= 3 (length (query l "tmp" :offset 0 :limit 3)))
  (is #'= 1 (length (query l "tmp" :offset 5 :limit 1)))
  (is #'= 0 (length (query l "tmp" :offset 0 :limit 0)))
  (fail (query l "tmp" :limit 2))
  (fail (query l "tmp" :offset 9)))

(define-library-test query-test-sort-and-direction (l p1 p2)
  (with-open-file (s p1 :direction :output :if-exists :supersede)
    ;; This might vary by library backend, but I think it makes sense
    ;; that most or all FTS engines would rank "hi hi" higher than
    ;; "hi" for a search of "hi", since it matches more often.
    (format s "hi hi >:3~%"))
  (index l p1)
  (sleep 1) ;; timestamps have a granularity of 1 second smdh
  (with-open-file (s p2 :direction :output :if-exists :supersede)
    (format s "hi :3~%"))
  (index l p2)
  ;; best match first
  (destructuring-bind (d1 d2)
      (query l "hi")
    (is #'equal (namestring p1) (datum-id d1))  ; fail
    (is #'equal (namestring p2) (datum-id d2))) ; fail
  ;; worst match first
  (destructuring-bind (d1 d2)
      (query l "hi" :direction :descending)
    (is #'equal (namestring p2) (datum-id d1))  ; fail
    (is #'equal (namestring p1) (datum-id d2))) ; fail
  ;; most recently modified first
  (destructuring-bind (d1 d2)
      (query l "hi" :sort-by :modified :direction :descending)
    (true (>= (datum-modified d1) (datum-modified d2))))
  ;; least recently modified first
  (destructuring-bind (d1 d2)
      (query l "hi" :sort-by :modified)
    (true (<= (datum-modified d1) (datum-modified d2))))
  ;; most recently created first
  (destructuring-bind (d1 d2)
      (query l "hi"  :sort-by :birth :direction :descending)
    (true (>= (datum-birth d1) (datum-birth d2))))
  ;; least recently created first
  (destructuring-bind (d1 d2)
      (query l "hi" :sort-by :birth)
    (true (<= (datum-birth d1) (datum-birth d2)))))

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

(define-library-test get-tag-data-test-sort-and-direction (l p1 p2)
  (index l p1)
    (sleep 1) ;; timestamps have a granularity of 1 second smh
  (with-open-file (s p2 :direction :output :if-exists :supersede)
    (format s "hi :3~%"))
  (index l p2)
  (add-datum-tags l p1 '("tag"))
  (add-datum-tags l p2 '("tag"))
  ;; most recently modified first
  (destructuring-bind (d1 d2)
      (get-tag-data l "tag" :sort-by :modified :direction :descending)
    (true (>= (datum-modified d1) (datum-modified d2))))
  ;; least recently modified first
  (destructuring-bind (d1 d2)
      (get-tag-data l "tag" :sort-by :modified :direction :ascending)
    (true (<= (datum-modified d1) (datum-modified d2))))
  ;; most recently created first
  (destructuring-bind (d1 d2)
      (get-tag-data l "tag" :sort-by :birth :direction :descending)
    (true (>= (datum-birth d1) (datum-birth d2))))
  ;; least recently created first
  (destructuring-bind (d1 d2)
      (get-tag-data l "tag" :sort-by :birth :direction :ascending)
    (true (<= (datum-birth d1) (datum-birth d2)))))

(define-library-test list-files-by-type (l p1 p2)
  (with-open-file (s p1 :direction :output :if-exists :supersede)
    (format s "hi :3~%"))
  (with-open-file (s p2 :direction :output :if-exists :supersede)
    (format s "<!DOCTYPE html>~%"))
  (index l (list p1 p2))
  (let ((res (library-list-files-by-type l "text/plain")))
    (is #'= 1 (length res))
    (is #'equal (namestring p1) (datum-id (car res))))
  (let ((res (library-list-files-by-type l "text/plain")))
    (is #'= 1 (length res))
    (is #'equal (namestring p1) (datum-id (car res)))))

;;; Working with files on disk

;; FIXME: verify that search terms changed!

(define-library-test move-datum-where-old-exists-new-doesnt (l oldp newp)
  (add-datum l (make-instance 'datum :id oldp))
  (delete-file newp)
  (add-datum-tags l oldp '("deez nuts"))
  (move-datum l oldp newp)
  (false (get-datum l oldp))
  (false (get-datum-tags l oldp))
  (is #'equal (namestring newp) (datum-id (get-datum l newp)))
  (is #'equal "deez nuts" (tag-name (car (get-datum-tags l newp)))))

(define-library-test move-datum-where-new-exists-on-disk-not-db (l oldp newp)
  (add-datum l (make-instance 'datum :id oldp))
  (delete-file oldp)
  (add-datum-tags l oldp '("таг"))
  (fail (move-datum l oldp newp)
      'datum-already-exists)
  (is #'equal (namestring newp) (move-datum l oldp newp :overwrite T))
  (false (get-datum l oldp))
  (is #'equal "таг" (tag-name (car (get-datum-tags l newp)))))

(define-library-test move-datum-where-both-exist (l oldp newp)
  (add-datum l (make-instance 'datum :id oldp))
  (fail (move-datum l oldp newp))
  (true (probe-file oldp))
  (true (probe-file newp))
  (add-datum l (make-instance 'datum :id newp))
  (delete-file newp)
  (fail (move-datum l oldp newp)
      'datum-already-exists)
  (true (probe-file oldp))
  (is #'equal (namestring newp) (move-datum l oldp newp :overwrite T))
  (false (probe-file oldp))
  (true (probe-file newp))
  (false (get-datum l oldp))
  (true (get-datum l newp)))

(define-library-test move-datum-where-old-not-indexed (l)
  (fail (move-datum l "no such datum" "also no such datum")
      'datum-not-indexed))

(define-library-test move-datum-where-theyre-the-same (l p)
  (fail (move-datum l p p)
      'cannot-mv-or-cp-to-itself)
  (true (probe-file p)))

(define-library-test move-datum-old-is-orphaned-and-not-renamed-on-disk (l p)
  (add-datum l (make-instance 'datum :id p))
  (delete-file p)
  (fail (move-datum l p "no such datum")
      'datum-is-orphaned))

(define-library-test copy-datum-where-old-exists-new-doesnt (l oldpath)
  (let ((d0 (car (index l oldpath)))
        (newpath (format NIL "~A_copy" oldpath)))
    (add-datum-tags l d0 '("To the town of Agua Fria rode a stranger one fine day"))
    (let ((d1 (copy-datum l oldpath newpath)))
      (false (leibowitz.core::%datum-equal d0 d1))
      (is #'equal newpath (datum-id d1))
      (is #'equal "To the town of Agua Fria rode a stranger one fine day"
          (tag-name (car (get-datum-tags l (datum-id d1))))))))

(define-library-test copy-datum-where-both-exist (l oldp newp)
  (index l oldp)
  ;; new on disk, not in db
  (fail (copy-datum l oldp newp) 'datum-already-exists)
  (false (get-datum l newp))
  ;; new in db, not on disk
  (index l newp)
  (delete-file newp)
  (fail (copy-datum l oldp newp) 'datum-already-exists)
  (false (probe-file newp))
  ;; new on both, no overwrite
  (let ((newp (format NIL "~A_copy" newp)))
    (uiop:copy-file oldp newp)
    (index l newp)
    (fail (copy-datum l oldp newp) 'datum-already-exists)
    (true (get-datum l newp))
    ;; now on both, overwrite
    (is #'equal (namestring newp) (datum-id (copy-datum l oldp newp :overwrite T)))))

(define-library-test copy-datum-where-old-not-indexed (l oldp)
  (fail (copy-datum l oldp (format NIL "~A_copy" oldp))
      'datum-not-indexed))

(define-library-test copy-datum-where-theyre-the-same (l p)
  (fail (copy-datum l p p) 'cannot-mv-or-cp-to-itself))

(define-library-test copy-datum-where-old-is-orphaned (l p)
  (index l p)
  (delete-file p)
  (fail (copy-datum l p (format NIL "~A_copy" p))
      'datum-is-orphaned))
