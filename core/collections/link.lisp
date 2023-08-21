;;; Collection subclass for managing on-disk representations of URI's

(in-package :leibowitz.core)

(defclass collection-link (collection)
  ()
  (:documentation "A collection of URIs that may or may not have been downloaded to
disk."))

(defmethod collection-index ((l library) (c collection-link) id)
  (check-type id string)
  (add-datum l (make-instance 'datum :id id :collection c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass collection-link/web (collection-link)
  ((directory
    :type (or pathname string)
    :accessor collection-link/web-directory
    :initarg :directory
    :documentation "Directory in which stored webpages are downloaded."))
  (:documentation "A collection of web links"))

(defmethod collection-applicable-p ((c collection-link/web) id)
  (etypecase id
    (string (if (equal (%datum-find-url-scheme-in-id id) "web")
                c
                NIL))
    (pathname NIL)))

;; FIXME: write methods for downloading URLs and stuff.  Probably
;; shell out to monolith to store everything in a single file

;; gemini, gopher, ftp, magnet, etc
