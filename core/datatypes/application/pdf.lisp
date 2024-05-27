;; Datum subclass for displaying pdfs

(in-package :leibowitz.core)

(defclass datum-application/pdf (datum)
  ()
  (:documentation "Datum implementation for pdfs."))

;; FIXME: extract PDF metadata and store it as named tags for display
;; in the sidebar!  Can we use :before/:after method chaining to call
;; this from the default sidebar method?  Or since they're slightly
;; special tags it would be easy to filter for them and display the
;; specially in the sidebar...

;; (defmethod datum-html-sidebar ((l library) (d datum-application/pdf)))
