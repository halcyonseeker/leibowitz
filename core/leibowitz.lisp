;; leibowitz.lisp — Define the core API for all library operations.

(in-package :leibowitz-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Central library API

(defclass library () ()
  (:documentation "The root-level data structure of a collection of tagged data."))

;;; Reading and writing data

(defgeneric add-datum (library datum)
  (:documentation "Given a datum, add it to the library then return it."))

(defgeneric get-datam (library id)
  (:documentation "Given a datum id, return an instance retrieved from the library or
NIL if it is not found.  ID may be either a string or a pathname."))

(defgeneric del-datum (library datum-or-id)
  (:documentation "Given a datum or its id in the form of a string or a pathname, remove
the corresponding entry from the library.  If the datum is not
present, do nothing."))

;;; Reading and writing tags

(defgeneric add-tag (library tag-or-name)
  (:documentation "Given a tag instance, insert it into the library.  This creates an
orphaned tag without any corresponding data, so generally speaking
you'll probably want to use `add-datum-tags' instead.  This method may
be useful for adding a tag with a label that gives it some kind of
special role."))

(defgeneric get-tag (library name)
  (:documentation "Find a tag by its name and return an instance of it or NIL if it
isn't present."))

(defgeneric del-tag (library tag-or-name)
  (:documentation "Given a tag or a tag name, delete it if it exists.  This will also
remove all data associations, but not the data themselves."))

;;; Reading and writing datum-tag relationships

(defgeneric get-datum-tags (library datum-or-id)
  (:documentation "Return a list of the tags associated with a datum where DATUM-OR-ID
is a `datum', a string, or a pathname.  Returns NIL if there are no tags."))

(defgeneric add-datum-tags (library datum-or-id tags)
  (:documentation "Add one or more tags to a datum."))

(defgeneric del-datum-tags (library datum-or-id tags &key cascade)
  (:documentation "Remove one or more tags from a datum.  If this leaves any tags
orphaned they will be removed unless they're stored with a label.  If
:cascade is T this will also remove all tags predicated in the tags to
be removed."))

(defgeneric get-tag-data (library tag-or-name)
  (:documentation "Return a list of the data associated with a tag or NIL if there isn't
any."))

;;; Reading and writing tag hierarchies

(defgeneric add-tag-predicate (library iftag-or-name thentag-or-name)
  (:documentation "Create a rule such that whenever the iftag is applied to a datum, the
thantag will be as well."))

(defgeneric get-tag-predicates (library tag-or-name)
  (:documentation "Return a list of tags to be applied to a datum with TAG."))

(defgeneric get-tag-predicands (library tag-or-name)
  (:documentation "Do the reverse of `get-tag-predicates' and return a list of tags that
require TAG also be applied."))

(defgeneric del-tag-predicate (library iftag-or-name thentag-or-name)
  (:documentation "Remove the requirement that data with the iftag must also have
thentag."))

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
    :initarg :kind
    :accessor datum-kind
    :documentation "This datum's type, for instance a file's mime type.")
   (birth
    :type bignum
    :initarg :birth
    :accessor datum-birth
    :documentation "The Epoch date at which this datum was created.")
   (modified
    :type bignum
    :initarg :modified
    :accessor datum-modified
    :documentation "The Epoch date at which this datum was last modified.")
   (terms
    :type string
    :initarg :terms
    :accessor datum-terms
    :documentation "A dump of textual terms to be used for full-text search."))
  (:documentation "The core unit of taggable data."))

(defmethod initialize-instance :after
    ((d datum) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (pathnamep (datum-id d))
    (setf (datum-id d) (namestring (datum-id d))))
  (unless (slot-boundp d 'kind)
    (setf (datum-kind d) (%datum-find-mime d)))
  (unless (slot-boundp d 'birth)
    (setf (datum-birth d) (%datum-find-birth d)))
  (unless (slot-boundp d 'modified)
    (setf (datum-modified d) (%datum-find-modified d)))
  (unless (slot-boundp d 'terms)
    (setf (datum-terms d) (%datum-find-terms d))))

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
    :initarg :count
    :accessor tag-count
    :initform 0
    :documentation "The number of data with this tag"))
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Curators keep the library up to date

(defclass curator () ()
  (:documentation "An optional and not-yet fully realized `library' companion that
manages external sources of data and makes sure the library is kept up
to date."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal utilities for testing and writing backends

(defgeneric %datum-equal (d1 d2)
  (:method ((d1 datum) (d2 datum))
    (and (equal (datum-id d1) (datum-id d2))
         (equal (datum-kind d1) (datum-kind d2))
         (equal (datum-birth d1) (datum-birth d2))
         (equal (datum-modified d1) (datum-modified d2))
         (equal (datum-terms d1) (datum-terms d2))))
  (:documentation "Return T if two datum instances are identical."))

(defun %need-tag-name (tag-or-name)
  (etypecase tag-or-name
    (tag (tag-name tag-or-name))
    (string tag-or-name)))

(defun %need-datum-id (datum-or-id)
  (etypecase datum-or-id
    (string datum-or-id)
    (pathname (namestring datum-or-id))
    (datum (datum-id datum-or-id))))

