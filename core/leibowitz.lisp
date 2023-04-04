;; leibowitz.lisp — Define the core API for all library operations.

(in-package :leibowitz-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Central library API

(defclass library () ()
  (:documentation "The root-level data structure of a collection of tagged data."))

;;; Reading and writing data

(defgeneric add-datum (library datum)
  (:documentation "Add a new datum to the library."))

(defgeneric get-datam (library id)
  (:documentation "Find a datum by its unique id."))

(defgeneric del-datum (library datum)
  (:documentation "Remove a datum from the library."))

;;; Reading and writing tags

(defgeneric add-tag (library tag)
  (:documentation "Create a new tag."))

(defgeneric get-tag (library name)
  (:documentation "Find a tag by its name."))

(defgeneric del-tag (library tag)
  (:documentation "Delete a tag."))

;;; Reading and writing datum-tag relationships

(defgeneric get-datum-tags (library datum)
  (:documentation "Return a list of the tags associated with a datum."))

(defgeneric add-datum-tags (library datum tags)
  (:documentation "Add one or more tags to a datum."))

(defgeneric del-datum-tags (library datum tags)
  (:documentation "Remove one or more tags from a datum."))

(defgeneric get-tag-data (library tag)
  (:documentation "Get the data associated with a tag."))

;;; Reading and writing tag hierarchies

(defgeneric add-tag-predicate (library iftag thentag)
  (:documentation "Add a predicate such that data with IFTAG will have THENTAG."))

(defgeneric get-tag-predicates (library tag)
  (:documentation "Return a list of tags to be applied to a datum with TAG."))

(defgeneric get-tag-predicands (library tag)
  (:documentation "Return a list of tags that require TAG also be applied."))

(defgeneric del-tag-predicate (library iftag thentag)
  (:documentation "Remove the requirement that data with IFTAG must have THENTAG."))

;;; Full-text search

(defgeneric query (library terms with-tags without-tags)
  (:documentation "Return data that match the search terms and tag filters."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data stored in the library

(defclass datum ()
  ((id
    :type string
    :initarg :id
    :accessor datum-id
    :initform (error "Datum ID required.")
    :documentation "The unique identifier of this piece of data.")
   (kind
    :type string
    :accessor datum-kind
    :documentation "This datum's type, for instance a file's mime type.")
   (birth
    :type bignum
    :accessor datum-birth
    :documentation "The Epoch date at which this datum was created.")
   (modified
    :type bignum
    :accessor datum-modified
    :documentation "The Epoch date at which this datum was last modified.")
   (terms
    :type string
    :accessor datum-terms
    :documentation "A dump of textual terms to be used for full-text search."))
  (:documentation "The core unit of taggable data."))

(defgeneric %datum-find-birth (datum)
  (:method ((d datum)) (declare (ignore d)) (get-universal-time))
  (:documentation "Find the date of this datum's creation.  POSIX doesn't specify this
so we'll just get the current time."))

(defgeneric %datum-find-terms (datum)
  (:method ((d datum)) (datum-id d))
  (:documentation "Find textual terms to search.  This will be specialized by subclasses
for different file types."))

(defgeneric %datum-find-modified (datum)
  (:method ((d datum)) (local-time:timestamp-to-universal
                        (local-time:unix-to-timestamp
                         (osicat-posix:stat-mtime
                          (osicat-posix:stat (datum-id d))))))
  (:documentation "Get the time when this datum's file was last modified."))

;; FIXME: This interacts with the file but failures aren't being
;; signaled as conditions
(defgeneric %datum-find-mime (datum)
  (:method ((d datum))
    (multiple-value-bind (stdout)
        (uiop:run-program (format NIL "file -i ~A" (datum-id d)) :output :string)
      (subseq stdout (+ 2 (search ":" stdout)) (search ";" stdout))))
  (:documentation "Get this mime time of this datum's file."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tags of stored data

(defclass tag ()
  ((name
    :type string
    :initarg :name
    :accessor tag-name
    :initform (error "A tag needs a NAME")
    :documentation "The name of this tag.")
   (label
    :type string
    :initarg :label
    :accessor tag-label
    :documentation "A textual description of this tag.")
   (count
    :type integer
    :accessor tag-count
    :documentation "The number of data with this tag"))
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Curators keep the library up to date

(defclass curator () ()
  (:documentation "An optional and not-yet fully realized `library' companion that
manages external sources of data and makes sure the library is kept up
to date."))
