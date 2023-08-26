;; leibowitz.lisp — Define the core API for all library operations.

(in-package :leibowitz.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Central library API

(defclass library ()
  ((thumbnail-cache-dir
    :type :pathname
    :initarg :thumbnail-cache-dir
    :initform (error "thumbnail-cache-dir required.")
    :accessor library-thumbnail-cache-dir
    :documentation "A directory in which to cache file thumbnails.")
   (collections
    :type list
    :initarg :collections
    :accessor library-collections
    :initform (list (make-instance 'collection-homedir)
                    (make-instance 'collection-link/web))
    :documentation "A list of cons cells mapping predicate
    functions to instances of collection classes.  Use the
    `library-get-datum-collection' method instead of querying these
    directly."))
  (:documentation "The root-level data structure of a collection of tagged data."))

(defmethod initialize-instance :after
    ((l library) &rest initargs &key &allow-other-keys)
  (ensure-directories-exist (library-thumbnail-cache-dir l))
  (let ((homedir (getf initargs :homedir))
        (linkdir (getf initargs :linkdir)))
    (when homedir
      (setf (collection-homedir-root
             (find-if (lambda (c) (eql (type-of c) 'collection-homedir))
                      (library-collections l)))
            homedir)
      (ensure-directories-exist homedir))
    (when linkdir
      (setf (collection-link/web-directory
             (find-if (lambda (c) (eql (type-of c) 'collection-link/web))
                      (library-collections l)))
            linkdir)
      (ensure-directories-exist linkdir))))

(defgeneric library-get-datum-collection (library id)
  (:method ((l library) id)
    (check-type id (or string pathname))
    (find-if (lambda (c) (collection-applicable-p c id)) (library-collections l)))
  (:documentation "Return the collection instance applicable to id.  This should be used
to populate the :collection slot when instantiating a datum."))

;;; Library metadata

(defgeneric library-data-quantity (library)
  (:documentation "Return the number of data stored in this library."))

;;; Reading and writing data

(defgeneric index (library path)
  (:documentation "High-level function to index a file, URL, or all files beneath a
directory tree."))

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

(defgeneric add-datum-tags (library datum-or-id tags &key replace)
  (:documentation "Add one or more tags to a datum.  If REPLACE is T the list of tags
replaces the existing ones rather than adding to them."))

;; FIXME: what about NIL?
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

;;; Searching and Listing

(defgeneric query (library terms &key limit offset)
  (:documentation "Return data that match the search terms.  :limit and :offset may be
used for pagination and are set to NIL by default, returning all
matching data."))

(defgeneric list-tags (library)
  (:documentation "Return a list of the top :limit sorted in descending order by
count."))

(defgeneric list-data (library &key direction sort-by offset limit)
  (:documentation "Return a list of data.  :direction may be either :ascending or
:descending and controls the manner in which the data are returned; it
defaults to :descending.  :sort-by controls the sorting criterion and
may be one of :modified, :birth, or :accesses, defaulting to
:modified.  :limit and :offset may be used for pagination and are set
to NIL by default.  That is all to say that calling this method
without any keys will return a list of all data sorted from the most
recently modified to the least recently modified."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data stored in the library

(defclass datum ()
  ((id
    :type string
    :initarg :id
    :accessor datum-id
    :initform (error "Datum ID required.")
    :documentation "The unique identifier of this piece of data.")
   (accesses
    :initarg :accesses
    :initform 0
    :accessor datum-accesses
    :documentation "Track how often the user accesses this datum.
    This slot must be manually incremented, it does not happen
    under-the-hood in get-datum.")
   (collection
    :initarg :collection
    :accessor datum-collection
    :documentation "A reference to the `collection' instance in `library' that manages this datum.")
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
  ;; Set the information required maybe to change our class
  (when (pathnamep (datum-id d))
    (setf (datum-id d) (namestring (datum-id d))))
  (unless (slot-boundp d 'kind)
    (let ((kind (%datum-find-url-scheme-in-id (datum-id d))))
      (if kind
          (setf (datum-kind d) (format NIL "link/~A" kind))
          (setf (datum-kind d) (%datum-find-mime d)))))
  ;; Change class to the appropriate one by mime type or URL scheme
  (handler-case
      (let ((full-mime (read-from-string
                        (format NIL "leibowitz.core:datum-~A" (datum-kind d)))))
        (change-class d (find-class full-mime)))
    (#+sbcl sb-int:simple-reader-package-error ()
      (handler-case
          (let ((major-mime (read-from-string
                             (format NIL "leibowitz.core:datum-~A"
                                     (subseq (datum-kind d)
                                             0 (search "/" (datum-kind d)))))))
            (change-class d (find-class major-mime)))
        (#+sbcl sb-int:simple-reader-package-error ()))))
  ;; Now set information with methods that might vary by class
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

(defgeneric %datum-find-mime (datum)
  (:method ((d datum))
    (multiple-value-bind (stdout)
        (uiop:run-program (list "file" "-Ei" (datum-id d)) :output :string)
      (subseq stdout (+ 2 (search ":" stdout)) (search ";" stdout))))
  (:documentation "Get this mime time of this datum's file."))

(defgeneric datum-title (datum)
  (:method ((d datum))
    (pathname-name (datum-id d)))
  (:documentation "Return a friendly title for this datum."))

(defgeneric datum-print-long-report (library datum)
  (:method ((l library) (d datum))
    (format T "   Datum: ~A~%" (datum-id d))
    (format T "    Type: ~A~%" (datum-kind d))
    (format T "Modified: ~A~%" (local-time:format-rfc1123-timestring
                                NIL (local-time:universal-to-timestamp
                                     (datum-modified d))))
    (format T "    Born: ~A~%" (local-time:format-rfc1123-timestring
                                NIL (local-time:universal-to-timestamp
                                     (datum-birth d))))
    (format T "    Tags: ~{~S~^, ~}~%" (loop for tag in (get-datum-tags l d)
                                                 collect (tag-name tag)))
    )
  (:documentation "Print a human-friendly summary of this datum."))

(defgeneric datum-html-report (library datum)
  (:method ((l library) (d datum))
    (declare (ignore l))
    `((:section
       (:pre ,(cl-who:escape-string
               (with-output-to-string (s)
                 (describe d s)))))))
  (:documentation "Return this datum summarized as a cl-who XHTML structure.  It should
consist of a list containing at least one section tag."))

(defgeneric datum-html-sidebar (library datum)
  (:method ((l library) (d datum))
    `(,(labels ((timefmt (universal)
                  (local-time:format-timestring
                   NIL (local-time:universal-to-timestamp universal)
                   :format '(:short-weekday ", " :short-month " " (:day 2) " "
                             (:year 4) " " (:hour 2) ":" (:min 2)))))
         `(:section (:h2 "About")
                    (:ul (:li (:span :class "datum-metadata-key"
                                     "Accesses")
                              (:span :class "datum-metadata-var"
                                     ,(format NIL "~A" (datum-accesses d))))
                         (:li (:span :class "datum-metadata-key"
                                     "Mime Type")
                              (:span :class "datum-metadata-var"
                                     ,(datum-kind d)))
                         (:li (:span :class "datum-metadata-key"
                                     "Collection")
                              (:span :class "datum-metadata-var"
                                     ,(format NIL "~A" (type-of (datum-collection d)))))
                         (:li (:span :class "datum-metadata-key"
                                     "Birth")
                              (:span :class "datum-metadata-var"
                                     ,(timefmt (datum-birth d))))
                         (:li (:span :class "datum-metadata-key"
                                     "Modified")
                              (:span :class "datum-metadata-var"
                                     ,(timefmt (datum-modified d)))))
                    (:a :href ,(format NIL "/raw?id=~A" (hunchentoot:url-encode (datum-id d)))
                        "View Raw")))
        ,(%collection-html-sidebar-section-for-datum l (datum-collection d) d)
        (:section (:h2 "Tags")
                  (:ul ,@(loop for tag in (get-datum-tags l d)
                               collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                                (hunchentoot:url-encode
                                                                 (tag-name tag)))
                                                 ,(tag-name tag))
                                             (:span :class "tag-count"
                                                    ,(format nil "(~a)" (tag-count tag)))))))))
    (:documentation "Return a cl-who XHTML structure holding some metatdata to be
displayed in the sidebar.  Like `datum-html-report', this should
consist of a list of sections."))

;; FIXME: add a list layout
(defgeneric datum-html-preview (library datum)
  (:method ((l library) (d datum))
    `(:div :class "tile"
           (:a :href ,(format NIL "/datum?id=~A"
                              (hunchentoot:url-encode (datum-id d)))
               ,(handler-case
                    `(:img :src ,(let ((thumbnailer:*thumbnail-cache-dir*
                                         (library-thumbnail-cache-dir l)))
                                   (format NIL "/thumbnail?path=~A"
                                           (hunchentoot:url-encode
                                            (namestring
                                             (thumbnailer:get-thumbnail
                                              (datum-id d) (datum-kind d)))))))
                  (thumbnailer:unsupported-file-type ()))
               (:div (:small ,(cl-who:escape-string (datum-id d)))))))
  (:documentation "Return a cl-who XHTML structure displaying a thumbnail preview of
this datum that links to the full detail page."))

(defgeneric injest-raw-datum (datum)
  (:method ((d datum))
    ;; FIXME: do collections need to have control over this?
    (with-open-file (s (datum-id d) :element-type '(unsigned-byte 8))
      (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
        (handler-case
            (loop for byte = (read-byte s)
                  for index from 0 to (file-length s)
                  do (setf (aref buf index) byte))
          (end-of-file () buf)))))
  (:documentation "Return a datum in its most primitive form, either an array of bytes
or a UTF-8 string."))

(defun %datum-find-url-scheme-in-id (id)
  "Little helper to find some kind of URL scheme in a datum's ID."
  (check-type id string)
  (let ((scheme (let ((pos (search ":" id)))
                  (when pos (subseq id 0 pos)))))
    (cond ((or (equal scheme "https")
               (equal scheme "http"))
           "web")
          (T scheme))))

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

(defgeneric tag-print-long-report (library tag)
  (:method ((l library) (tag tag))
    (format T "  Tag: ~A~%" (tag-name tag))
    (format T "Label: ~A~%" (tag-label tag))
    (format T " Data: ~{~S~^, ~}~%" (loop for datum in (get-tag-data l tag)
                                          collect (datum-id datum))))
  (:documentation "Print a human-friendly summary of this tag."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collections

(defclass collection () ()
  (:documentation "Each datum stored in a library is part of a collection.  Unlike tags,
collections are mutually exclusive and govern how each datum's id
corresponds with files on disk."))

(defgeneric collection-applicable-p (collection id)
  (:documentation "Check if this collection is applicable to a given ID."))

(defgeneric collection-index (library collection id)
  (:documentation "Index a file or url into the library per the rules of this
collection."))

(defgeneric %collection-html-sidebar-section-for-datum (library collection datum)
  (:method ((l library) (c collection) (d datum))
    NIL)
  (:documentation "Return an XHTML <section></section> as a cl-who structur which will
be slotted into the sidebar of this datum's report page.  The
intention is this might be used to display collection-specific
metadata about this datum, for instance the artist name and source URL
in a hypothetical collection managing a gallery-dl archive."))

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
         (equal (datum-terms d1) (datum-terms d2))
         (eq (datum-collection d1) (datum-collection d2))))
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

