;;; Datum subclass for URIs that may or may not correspond to files.

(in-package :leibowitz.core)

(defclass datum-link (datum)
  ()
  (:documentation "Parent class for data that lack an underlying file."))

;; FIXME: we'll specialize this by link type from now on
(defmethod datum-title ((d datum-link))
  (datum-id d))

;; FIXME: add a list layout
(defmethod datum-html-preview ((l library) (d datum-link) &key view)
  (declare (ignore view))
  `(:div :class "tile"
         (:a :href ,(format NIL "/datum?id=~A"
                            (hunchentoot:url-encode (datum-id d)))
             (:div (:small ,(cl-who:escape-string (datum-id d)))))))

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
