;;; The default collection, used for files in the user's home

(in-package :leibowitz.core)

(defclass collection-homedir (collection)
  ((root
    :type pathname
    :initarg :root
    :initform (truename (user-homedir-pathname))
    :accessor collection-homedir-root)
   (includes
    :type list
    :initarg :includes
    :initform '("*")
    :accessor collection-homedir-includes
    :documentation "")
   (excludes
    :type list
    :initarg :excludes
    :initform '(".*")
    :accessor collection-homedir-excludes
    :documentation ""))
  (:documentation ""))

(defmethod print-object ((c collection-homedir) stream)
  (print-unreadable-object (c stream :type T)
    (format stream "~A" (collection-homedir-root c))))

(defmethod collection-applicable-p ((c collection-homedir) id)
  (check-type id (or string pathname))
  (if (eql 0 (search (namestring (collection-homedir-root c)) (namestring id)))
      c
      NIL))

(defmethod collection-index ((l library) (c collection-homedir) id)
  (check-type id (or string pathname))
  (add-datum l (make-instance 'datum :id id :collection c)))

;; FIXME: write the knowability method
