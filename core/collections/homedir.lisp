;;; The default collection, used for files in the user's home

(in-package :leibowitz-core)

(defclass collection-homedir (collection)
  ((root
    :type pathname
    :initarg :root
    :initform (user-homedir-pathname)
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

(defmethod collection-applicable-p ((c collection-homedir) id)
  (check-type id (or string pathname))
  (if (search (namestring (collection-homedir-root c)) (namestring id))
      c
      NIL))

;; FIXME: write the knowability method
