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
    :initform (list (make-instance 'collection-homedir))
    :documentation "A list of cons cells mapping predicate
    functions to instances of collection classes.  Use the
    `library-get-datum-collection' method instead of querying these
    directly."))
  (:documentation "The root-level data structure of a collection of tagged data."))

(defmethod initialize-instance :after
    ((l library) &rest initargs &key &allow-other-keys)
  (ensure-directories-exist (library-thumbnail-cache-dir l))
  (let ((homedir (getf initargs :homedir)))
    (when homedir
      (setf (collection-homedir-root
             (find-if (lambda (c) (eql (type-of c) 'collection-homedir))
                      (library-collections l)))
            homedir)
      (ensure-directories-exist homedir))))

(defgeneric library-get-datum-collection (library id)
  (:method ((l library) (id string))
    (let ((col (find-if (lambda (c) (collection-applicable-p c id))
                        (library-collections l))))
      (if col col (error 'no-applicable-collection :id id))))
  (:method ((l library) (id pathname))
    (library-get-datum-collection l (uiop:native-namestring id)))
  (:documentation "Find the collection instance which manages ID.  If none can be found,
raise an error of type `no-applicable-collection'.  This should be
used to populate the :collection slot when instantiating a datum."))

(defgeneric library-print-info (library)
  (:documentation "Print information about this library."))

;;; Library metadata

(defgeneric library-data-quantity (library)
  (:documentation "Return the number of data stored in this library."))

(defgeneric library-tag-quantity (library)
  (:documentation "Return the number of tags in the library."))

(defgeneric library-all-file-types (library)
  (:documentation "Return a list of cons cells where the car is each unique file type in
the library and where the cdrs is the quantity of data with that type."))

;; FIXME: unify searching and listing files into a single method!
;; FIXME: consider adding support for matching the major/ and + parts
;; of the mime type.
(defgeneric library-list-files-by-type (library type)
  (:documentation "Return a list of data whose `datum-kind' slot matches TYPE."))

;; FIXME: Like the method right above, this method really needs to be
;; merged in with list-data once I figure out how to pass SQLite a
;; parameter into a LIKE match without getting a gaping huge SQL
;; injection.  For now this suffices...  Also note that since we're
;; working against the filesystem for this right now this will fail to
;; return any data whose files have been moved or deleted.
(defgeneric library-list-files-in-dir (library dir &key include-unindexed)
  (:documentation "Return a list of data in DIR.  If :include-unindexed is true un
unindexed files will be included as pathnames, so the caller will need
to typecase on the result.  Note that it's possible that some data
returned by this method will have since been deleted or renamed on the
filesystem."))

(defgeneric datum-num-tags (library datum)
  (:documentation "Return the number of tags associated with this datum."))

(defgeneric library-path-indexable-p (library path)
  (:documentation "Given a path, return T or NIL depending on whether or not it's allowed
to be indexed.  At some point this will evolve into a full
include/exclude system, akin to gitignore, but for right now we're
just flat excluding all hidden files and children of hidden
directories.")
  (:method ((l library) (p pathname))
    (if (loop for part in (append (cdr (pathname-directory p))
                                  (list (if (pathname-name p) (pathname-name p) " ")))
              when (eq #\. (aref part 0)) do (return T))
        NIL
        T)))

;;; Reading and writing data

(defgeneric index (library path-or-paths &key log promote-error)
  (:documentation "High-level function to index a file, a list of files, or all files
beneath a directory tree.  Logs progress to stdout if :log is T; if
:promote-error is T recoverable errors encountered during indexing
will be promoted to the user, if not they will be logged to stderr
regardless of the value of :log.  Index returns a list of all the
files indexed."))

(defgeneric add-datum (library datum)
  (:documentation "Given a datum, add it to the library then return it."))

(defgeneric get-datum (library id &key error)
  (:documentation "Given a datum id, return an instance retrieved from the library or
NIL if it is not found.  ID may be either a string or a pathname.  If
:error is T, raise a condition of type `datum-not-indexed' rather than
returning NIL."))

(defgeneric del-datum (library datum-or-id &key error disk)
  (:documentation "Given a datum or its id in the form of a string or a pathname, remove
the corresponding entry from the library.  If the datum is not
present, do nothing unless :error is T, in which case raise a
`datum-not-indexed'.  If :disk is NIL, suppress the default behavior
of removing the underlying file.  If the underlying file is not
present and :disk is T, yield an error of type `datum-is-orphaned'."))

(defgeneric move-datum (library old-datum-or-id new-datum-or-id &key overwrite)
  (:documentation "Move or rename a datum, preserving its thumbnail, metadata, and tag
elationships.  Generally this will be used where OLD exists on disk
and NEW doesn't, however it also works for the inverse in the case
where the user moved the file on disk and doesn't want to use
metadata.  If both exist, an error is issued unless :overwrite is T.
If neither exist or they're the same, an error is issued."))

(defgeneric copy-datum (library old-datum-or-id new-datum-or-id &key overwrite)
  (:documentation "Copy one datum to another name, keeping the old version and preserving
all metadata and thumbnails, returning the new datum.  If NEW exists,
either in the database or on disk, an error will be issued unless
:overwrite is T.  If OLD doesn't exist in both database and disk an
error will be issued."))

;;; Reading and writing tags

(defgeneric add-tag (library tag-or-name)
  (:documentation "Given a tag instance, insert it into the library.  This creates an
orphaned tag without any corresponding data, so generally speaking
you'll probably want to use `add-datum-tags' instead.  This method may
be useful for adding a tag with a label that gives it some kind of
special role.  Returns the tag instance."))

(defgeneric get-tag (library tag-name &key error)
  (:documentation "Find a tag by its name and return an instance of it.  By default
returns NIL if the tag isn't found, if :error is T, raise a
`no-such-tag'."))

(defgeneric del-tag (library tag-or-name)
  (:documentation "Given a tag or a tag name, delete it if it exists.  This will also
remove all data associations, but not the data themselves."))

(defgeneric move-tag (library old-tag-or-name new-tag-or-name &key overwrite merge)
  (:documentation "Move a tag to a new name, retaining all datum and predicate
associations.  If NEW exists and :overwrite isn't T, an error of type
`tag-already-exists' will be signaled.  If NEW exists and :merge is T,
OLD and NEW will be merged together into NEW, retaining the datum and
predicate associations of OLD and NEW.  The key difference between
:overwrite and :merge is that when the former is provided, NEW is
first deleted in order to remove all predicate and data associations;
ie, it is a clean rename.  Passing both keys is an error.  Returns
NEW."))

(defgeneric copy-tag (library old-tag-or-name new-tag-or-name &key overwrite merge)
  (:documentation "Duplicate tag OLD to tag NEW, so that both have the same datum and
predicate associations.  If NEW exists and :overwrite isn't T, an
error of type `tag-already-exists' is signaled.  If NEW exists and
:merge is T, the copy will be merged into that tag, retaining all
datum and predicate associations of OLD and NEW, while still keeping
OLD around.  Passing both keys is an error Returns NEW."))

;;; Reading and writing datum-tag relationships

(defgeneric get-datum-tags (library datum-or-id)
  (:documentation "Return a list of the tags associated with a datum where DATUM-OR-ID
is a `datum', a string, or a pathname.  Returns NIL if there are no tags."))

(defgeneric add-datum-tags (library datum-or-id tags &key replace)
  (:documentation "Add one or more tags to a datum.  If REPLACE is T the list of tags
replaces the existing ones rather than adding to them.  An error of type
`datum-not-indexed' is signaled if the supplied one isn't present."))

(defgeneric del-datum-tags (library datum-or-id tags &key cascade)
  (:documentation "Remove one or more tags from a datum.  If this leaves any tags
orphaned they will be removed unless they're stored with a label.  If
:cascade is T this will also remove all tags predicated in the tags to
be removed.  Signals `datum-not-indexed' if the requested datum cannot
be found."))

;; FIXME: unify searching and listing files into a single method!
(defgeneric get-tag-data (library tag-or-name &key sort-by direction limit offset)
  (:documentation "Return a list of the data associated with a tag or NIL if there isn't
any.  Keyword arguments function the same as `list-data' and `query'"))

;;; Reading and writing tag hierarchies

(defgeneric add-tag-predicate (library iftag-or-name thentags-or-names
                               &key retroactive replace)
  (:documentation
   "Create a rule such that whenever the iftag is applied to a datum,
then each tag in thantags will be as well.  RETROACTIVE is T by
default, meaning that all the data already with IFTAG will also get
THENTAG unless you specify otherwise.  If REPLACE is T, then the list
of thentags replaces the existing subtags rather than adding to them."))

(defgeneric get-tag-predicates (library tag-or-name)
  (:documentation "Return a list of tags to be applied to a datum with TAG."))

(defgeneric get-tag-predicands (library tag-or-name)
  (:documentation "Do the reverse of `get-tag-predicates' and return a list of tags that
require TAG also be applied."))

(defgeneric del-tag-predicate (library iftag-or-name thentag-or-name)
  (:documentation "Remove the requirement that data with the iftag must also have
thentag."))

;;; Searching and Listing

;; FIXME: unify searching and listing files into a single method!
(defgeneric query (library terms &key sort-by direction limit offset)
  (:documentation "Return data that match the search terms.  :sort-by and :direction
behave the same as for `list-data', except that the former also
accepts a :rank value to order by how well the search matches, and
that the latter defaults to :ascending in order to show the most
relevant search matches by default.  :limit and :offset may be used
for pagination and are set to NIL by default, returning all matching
data."))

(defgeneric list-tags (library &key limit)
  (:documentation
   "Return a list of the top :limit tags sorted by number of data
in descending order.  Returns all by default."))

;; FIXME: unify searching and listing files into a single method!
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

(defmethod print-object ((datum datum) stream)
  (print-unreadable-object (datum stream :type T)
    (format stream "~A" (datum-id datum))))

(defmethod initialize-instance :after
    ((d datum) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  ;; Set the information required maybe to change our class
  (setf (datum-id d) (%need-datum-id (datum-id d)))
  (unless (slot-boundp d 'kind)
    ;; FIXME: What about a policy for symlink and hardlink resolution?
    ;; As it stands the following condition means that (make-instance
    ;; 'datum ...) will fail for symlinks, is this what we want?
    ;;; Make sure it's a regular file.
    (unless (osicat-posix:s-isreg
             (osicat-posix:stat-mode (osicat-posix:stat (datum-id d))))
      (error 'file-not-regular :path (datum-id d)))
    (setf (datum-kind d) (%datum-find-mime d)))
  ;; Change class to the appropriate one by mime type
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
      (let ((mime (subseq stdout (+ 2 (search ": " stdout :from-end T))
                          (search "; " stdout :from-end T)))
            (cbz (subseq (datum-id d) (- (length (datum-id d)) 4) (length (datum-id d)))))
        ;; Work around the fact that file 5.45 shipped by OpenSuse
        ;; Tumbleweed yields "application/zip" for all the comic book
        ;; archive files in my library.
        (if (equal cbz ".cbz")
            "application/vnd.comicbook+zip"
            mime))))
  (:documentation "Get this mime time of this datum's file."))

(defgeneric datum-title (datum)
  (:method ((d datum))
    (pathname-name (uiop:parse-native-namestring (datum-id d))))
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
       (:pre ,(html
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
                    (:ul (:li (:span :class "sidebar-metadata-key"
                                     "Accesses")
                              (:span :class "sidebar-metadata-var"
                                     ,(format NIL "~A" (datum-accesses d))))
                         (:li (:span :class "sidebar-metadata-key"
                                     "Mime Type")
                              (:span :class "sidebar-metadata-var"
                                     (:a :href ,(format NIL "/type/~A" (datum-kind d))
                                         ,(datum-kind d))))
                         (:li (:span :class "sidebar-metadata-key"
                                     "Collection")
                              (:span :class "sidebar-metadata-var"
                                     ,(format NIL "~A" (type-of (datum-collection d)))))
                         (:li (:span :class "sidebar-metadata-key"
                                     "Birth")
                              (:span :class "sidebar-metadata-var"
                                     ,(timefmt (datum-birth d))))
                         (:li (:span :class "sidebar-metadata-key"
                                     "Modified")
                              (:span :class "sidebar-metadata-var"
                                     ,(timefmt (datum-modified d)))))
                    (:a :id "raw-url"
                        :href ,(format NIL "/raw?id=~A" (url (datum-id d)))
                        "View Raw")))
        ,(%collection-html-sidebar-section-for-datum l (datum-collection d) d)
        (:section (:h2 ,(format NIL " Tags (~A)" (datum-num-tags l d)))
                  (:ul ,@(loop for tag in (get-datum-tags l d)
                               collect `(:li (:a :href ,(format NIL "/tag?name=~A"
                                                                (url (tag-name tag)))
                                                 ,(tag-name tag))
                                             (:span :class "tag-count"
                                                    ,(format nil "(~a)" (tag-count tag)))))))))
    (:documentation "Return a cl-who XHTML structure holding some metatdata to be
displayed in the sidebar.  Like `datum-html-report', this should
consist of a list of sections."))

(defgeneric datum-html-preview (library datum &key view)
  (:method ((l library) (d datum) &key (view :tile))
    (ecase view
      (:tile
       `(:div :class "tile"
              (:a :href ,(format NIL "/datum?id=~A" (url (datum-id d)))
                  :title ,(let ((nt (datum-num-tags l d)))
                            (format NIL "~A; ~A tag~P" (datum-kind d) nt nt))
                  ,(handler-case
                       `(:img :src ,(let ((thumbnailer:*thumbnail-cache-dir*
                                            (library-thumbnail-cache-dir l)))
                                      (format NIL "/thumbnail?path=~A"
                                              (url (uiop:native-namestring
                                                    (thumbnailer:get-thumbnail
                                                     (datum-id d) (datum-kind d)))))))
                     (thumbnailer:source-file-not-accessible ()
                       `(:p "This file has been moved or deleted on disk!"))
                     (thumbnailer:unsupported-file-type ())
                     (thumbnailer:thumbnail-creation-failed ()))
                  (:div (:small ,(html (datum-title d)))))))
      (:card
       `(:div :class "card"
              (:div :class "card-view" ,@(datum-html-report l d))
              (:hr)
              (:div :class "card-data"
                    (:h2 ,(html (datum-title d)))
                    (:a :href ,(format NIL "/datum?id=~A" (url (datum-id d)))
                        (:small ,(html (datum-id d)))))))))
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

(defmethod print-object ((tag tag) stream)
  (print-unreadable-object (tag stream :type T)
    (format stream "~A" (tag-name tag))))

(defgeneric tag-print-long-report (library tag)
  (:method ((l library) (tag tag))
    (format T "  Tag: ~A~%" (tag-name tag))
    (format T "Label: ~A~%" (tag-label tag))
    (format T " Data: ~{~S~^, ~}~%" (loop for datum in (get-tag-data l tag)
                                          collect (datum-id datum))))
  (:documentation "Print a human-friendly summary of this tag."))

(defgeneric tag-num-parents (library tag-or-name)
  (:documentation "Return the number of tags this tag automatically adds."))

(defgeneric tag-num-children (library tag-or-name)
  (:documentation "Return the number of tags that automatically add this tag"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collections

(defclass collection () ()
  (:documentation "Each datum stored in a library is part of a collection.  Unlike tags,
collections are mutually exclusive and govern how each datum's id
corresponds with files on disk."))

(defgeneric collection-applicable-p (collection id)
  (:documentation "Check if this collection is applicable to a given ID."))

(defgeneric collection-index (library collection id)
  (:documentation "Index a file into the library per the rules of this
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

#+windows(error "pathname normalization will need to be rewritten for windows")
#-windows(defun %need-datum-id (datum-or-id)
  "Given a datum, a string, or a pathname, return the appropriate datum
id.  This is possibly the most important function in this package.  It
is responsible for normalizing paths provided as input by the user
into strings containing the absolute (for now…) paths that uniquely
identify indexed data.  It works around CL's God-awful wild pathname
facility and does its best to produce absolute paths even when the
file doesn't exist on disk.

God and the machine spirits willing, when we switch to addressing
files by paths relative to $HOME or $LEIBOWITZ_ROOT so that libraries
may be portable, this function will be _the only_ code in Leibowitz
outside of `library' itself that will need to change in order for all
datum and library methods to work 🙏⛩️"
  (etypecase datum-or-id
    ((or string pathname)
     ;; Notice how we're using `uiop:native-namestring' and
     ;; `uiop:parse-native-namestring' instead of `namestring' and
     ;; `pathname' respectively.  This is necessary because CL's
     ;; pathname type reeks worse than a week old corpse in summer; in
     ;; SBCL at least, it insists on interpreting square brackets in
     ;; file names as wildcards with no clear way to DISABLE
     ;; WILDCARDING AND ADDRESS THE FILE LITERALLY, GOD DAMMIT.  With
     ;; all the thought that went in to the design of CL you'd expect
     ;; this might have crossed someone's mind at some point 🙃.  Oh,
     ;; also, there are probably a bunch of places in the core waiting
     ;; to explode because of this
     (uiop:native-namestring
      (let ((p (uiop:parse-unix-namestring datum-or-id)))
        ;; There ~shouldn't~ be any difference between these but this
        ;; may be a source of fiddly annoying bugs...
        (if (probe-file p) (truename p) (merge-pathnames p)))))
    (datum (datum-id datum-or-id))))

