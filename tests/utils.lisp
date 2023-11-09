
(in-package :leibowitz/tests)

(defmacro with-tmp-files ((&rest tmpfiles) &body body)
  `(let (,@(loop for var in tmpfiles
                 collect `(,var (uiop:tmpize-pathname
                                 #P"/tmp/leibowitz_test_tmpfile"))))
     (unwind-protect (progn ,@body)
       ,@(loop for var in tmpfiles
               collect `(delete-file ,var)))))

(defun mktmp (&optional (prefix #P"/tmp/"))
  "Create a temporary file in /tmp/, or under PREFIX if it's specified
and return the pathname.  Unlike `with-tmp-files', the caller is
responsible for cleanup."
  (uiop:tmpize-pathname (merge-pathnames prefix "leibowitz_test_tmpfile")))

