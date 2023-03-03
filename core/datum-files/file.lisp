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

;;; (defmethod datum-find-birth ((f datum-file))
;; There isn't a standard way of encoding a file's birth time (ie,
;; this isn't returned by a stat(2) syscall), so for now at least
;; we'll fall back to the default method.

(defmethod datum-find-modified ((f datum-file))
  ;; Everything that actually interacts with the file needs to
  ;; suppress nonexistant-file errors in order to allow throwaway
  ;; dummy instances to be instantiated to dispatch schema
  (handler-case
      (local-time:timestamp-to-universal
       (local-time:unix-to-timestamp
        (osicat-posix:stat-mtime
         (osicat-posix:stat (datum-id f)))))
    (t () (get-universal-time))))

;; Later on initialize-instance will use the mime slot to change its
;; own class definition to some subclass of datum-file that
;; specializes this method on a specific file type.
(defmethod datum-find-terms ((f datum-file))
  (namestring (datum-id f)))

(defmethod datum-file-find-mime ((f datum-file))
  ;; This interacts with the file but failures aren't being signaled
  ;; as conditions so 💀
  (multiple-value-bind (stdout)
      (uiop:run-program (format NIL "file -i ~A" (datum-id f)) :output :string)
    (subseq stdout (+ 2 (search ":" stdout)) (search ";" stdout))))

(defmethod schema ((f datum-file) (l sqlite-library))
  (declare (ignore l))
  "
create table if not exists 'data' (
  'id' text not null unique,
  'birth' datetime not null,
  'modified' datetime not null,
  'terms' text,
  'mime' text not null
)")
