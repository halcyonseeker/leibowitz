
(defpackage :leibowitz-core
  (:use #:cl)
  ;; Basic administrative API
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
           #:del-tag-predicate)
  ;; Search and Listing
  (:export #:query
           #:list-tags
           #:list-data)
  ;; A unit of tagable data
  (:export #:datum
           #:datum-id
           #:datum-kind
           #:datum-birth
           #:datum-modified
           #:datum-terms)
  ;; Specific types of data
  (:export #:datum-text
           #:datum-text/html)
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
           #:with-sqlite-tx
           )
  ;; Curator implementations
  (:export #:fs-curator)
  )
