
(defpackage :leibowitz-core
  (:use #:cl)
  ;; High-level APi
  (:export #:library
           #:add-datum
           #:get-datum
           #:del-datum
           #:add-tag
           #:get-tag
           #:del-tag
           #:get-datum-tags
           #:add-datum-tags
           #:del-datum-tags
           #:get-tag-data
           #:add-tag-predicate
           #:get-tag-predicates
           #:get-tag-predicands
           #:del-tag-predicates
           #:query)
  ;; A unit of tagable data
  (:export #:datum
           #:datum-id
           #:datum-birth
           #:datum-modified
           #:datum-terms
           #:datum-find-birth
           #:datum-find-modified
           #:datum-find-terms)
  ;; Tag metadata
  (:export #:tag
           #:tag-name
           #:tag-count)
  ;; Library curator API
  (:export #:curator)
  ;; SQLite library backend
  (:export #:sqlite-library
           #:sqlite-nq
           #:sqlite-row
           #:sqlite-rows
           )
  ;; Datum types
  (:export #:datum-file
           #:datum-file-find-mime)
  ;; Curator implementations
  (:export #:fs-curator)
  )
