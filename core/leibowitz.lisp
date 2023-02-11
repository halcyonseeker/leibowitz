;; leibowitz.lisp — Define the core API for all library operations.

(in-package :leibowitz-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass library ()
  ((member-datum
    :initarg :member-datum
    :initform 'datum
    :accessor library-member-datum
    :documentation "A type specifier denoting the kind of `datum' this library stores."))
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

(defclass datum ()
  ((id
    :type string
    :initarg :id
    :accessor datum-id
    :initform (error "Datum ID required.")
    :documentation "The unique identifier of this piece of data.")
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

(defgeneric datum-find-birth (datum)
  (:method ((d datum)) (declare (ignore d)) (get-universal-time))
  (:documentation "Find the date of creation."))

(defgeneric datum-find-modified (datum)
  (:method ((d datum)) (declare (ignore d)) (get-universal-time))
  (:documentation "Find the date of last modification."))

(defgeneric datum-find-terms (datum)
  (:method ((d datum)) (datum-id d))
  (:documentation "Find textual terms to search."))

(defmethod initialize-instance :after
    ((d datum) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (datum-birth d) (datum-find-birth d))
  (setf (datum-modified d) (datum-find-modified d))
  (setf (datum-terms d) (datum-find-terms d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defclass curator () ()
  (:documentation "An optional and not-yet fully realized `library' companion that
manages external sources of data and makes sure the library is kept up
to date."))
