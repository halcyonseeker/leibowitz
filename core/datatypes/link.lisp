;;; Datum subclass for URIs that may or may not correspond to files.

(in-package :leibowitz.core)

(defclass datum-link (datum)
  ()
  (:documentation "Parent class for data that lack an underlying file."))

(defmethod %datum-find-modified ((d datum-link))
  "Since stored links don't correspond directly to an underlying file
there isn't any modification time we can infer."
  (declare (ignore d))
  (get-universal-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass datum-link/web (datum-link)
  ()
  (:documentation "Datum implementation for web links; anything where the `datum-id'
slot starts with http:// or https://."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: we should figure out a policy for resolving file:// links
