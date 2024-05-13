;;; Datum subclass for displaying images

(in-package :leibowitz.core)

(defclass datum-image (datum)
  ()
  (:documentation "Datum implementation for files with mime type
  image/*."))

;; FIXME: write a %datum-find-terms that shells out to an EasyOCR
;; wrapper to extract text from the image :)

(defmethod datum-html-report ((l library) (d datum-image))
  (declare (ignore l))
  `((:section
     (:img :alt (html "Once we get OCR support that'll be the alt text :)")
           :src ,(format NIL "/raw?id=~A" (url (datum-id d)))))))
