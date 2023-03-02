;; A library backend to store the data in files on disk

(in-package :leibowitz-core)

(defclass datum-file (datum)
  ((mime
    :type string
    :accessor datum-file-mime
    :documentation "This file's mime type."))
  (:documentation "A unit of tagable data corresponding to a file on disk."))

(defmethod initialize-instance :after
    ((f datum-file) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (datum-file-mime f) (datum-file-find-mime f))
  ;; Datum IDs should always be strings, since in this case it's a
  ;; file path it might be passed in as a pathname instead.
  (setf (datum-id f) (namestring (datum-id f)))
  ;; I think we can implement file-type specific behavior here by
  ;; searching for a class named datum-file-major/minor and redefining
  ;; this instance's class to it.
  )

(defmethod datum-find-birth ((f datum-file)))

(defmethod datum-find-modified ((f datum-file)))

(defmethod datum-find-terms ((f datum-file)))

(defmethod datum-file-find-mime ((f datum-file)))
