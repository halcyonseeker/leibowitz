
(in-package :leibowitz/tests)

(defmacro with-tmp-files ((&rest tmpfiles) &body body)
  `(let (,@(loop for var in tmpfiles
                 collect `(,var (uiop:tmpize-pathname
                                 #P"/tmp/leibowitz_test_tmpfile"))))
     (unwind-protect (progn ,@body)
       ,@(loop for var in tmpfiles
               collect `(delete-file ,var)))))

(defun mktmp (&optional (prefix #P"/tmp/") (type :file))
  "Create a temporary file in /tmp/, or under PREFIX if it's specified
and return the pathname.  Unlike `with-tmp-files', the caller is
responsible for cleanup."
  (assert (member type '(:file :dir :fifo)))
  (let ((path (merge-pathnames
               (format NIL "leibowitz_test_tmp~A-~A"
                       (string-downcase type) (random (expt 36 8)))
               prefix)))
    (case type
      (:file (osicat-posix:creat path osicat-posix:*default-open-mode*))
      (:dir  (ensure-directories-exist (format NIL "~A/" path)))
      (:fifo (osicat-posix:mkfifo path osicat-posix:*default-open-mode*)))
    path))

