;;; Datum subclass for displaying videos

(in-package :leibowitz.core)

(defclass datum-video (datum)
  ()
  (:documentation "Datum implementation for files with mime type video/*."))

;; FIXME: write a %datum-find-terms that extracts textual information
;; from the video's metadata :)

(defmethod datum-html-report ((l library) (d datum-video))
  (declare (ignore l))
  `((:section
     (:video :controls T
             :src ,(format NIL"/raw?id=~A"
                           (hunchentoot:url-encode (datum-id d)))
             :type ,(datum-kind d)))))
