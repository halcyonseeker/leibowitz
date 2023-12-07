
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
  (assert (member type '(:file :dir)))
  (case type
    (:file (uiop:tmpize-pathname
            (merge-pathnames prefix "leibowitz_test_tmpfile")))
    (:dir (ensure-directories-exist
           (merge-pathnames
            (format NIL "leibowitz_test_tmpdir-~A/" (random (expt 36 8)))
            prefix)))))

