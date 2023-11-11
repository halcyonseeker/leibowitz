
(defpackage :leibowitz.core
  (:use #:cl)
  ;; Basic administrative API
  (:export #:library
           #:library-thumbnail-cache-dir
           #:library-collections
           #:library-print-info
           #:library-data-quantity
           #:library-all-file-types
           #:library-get-datum-collection)
  (:export #:index
           #:injest-raw-datum
           #:add-datum
           #:get-datum
           #:del-datum
           #:move-datum
           #:copy-datum
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
  ;; Display methods
  (:export #:datum-print-long-report
           #:datum-html-report
           #:datum-html-sidebar
           #:datum-html-preview)
  ;; Search and Listing
  (:export #:query
           #:list-tags
           #:list-data)
  ;; A unit of tagable data
  (:export #:datum
           #:datum-id
           #:datum-accesses
           #:datum-collection
           #:datum-kind
           #:datum-birth
           #:datum-modified
           #:datum-terms
           #:datum-title
           #:datum-num-tags)
  ;; Specific types of data
  (:export #:datum-text
           #:datum-text/html
           #:datum-image
           #:datum-video
           #:datum-link/web)
  ;; Tag metadata
  (:export #:tag
           #:tag-name
           #:tag-label
           #:tag-count
           #:tag-print-long-report)
  ;; Mutually-exclusive kinds of data
  (:export #:collection
           #:collection-applicable-p
           #:collection-index)
  ;; Types of collectionsn
  (:export #:collection-homedir
           #:collection-homedir-root
           #:collection-link
           #:collection-link/web)
  ;; Library curator API
  (:export #:curator)
  ;; Error conditions
  (:export #:friendly-error
           #:datum-not-indexed
           #:datum-already-exists
           #:cannot-mv-or-cp-to-itself
           #:no-such-datum-in-disk-or-db
           #:datum-is-orphaned
           #:no-applicable-collection)
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
