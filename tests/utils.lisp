
(in-package :leibowitz/tests)

(defmacro with-tmp-files ((&rest tmpfiles) &body body)
  `(let (,@(loop for var in tmpfiles
                 collect `(,var (uiop:tmpize-pathname
                                 #P"/tmp/leibowitz_test_tmpfile"))))
     (unwind-protect (progn ,@body)
       ,@(loop for var in tmpfiles
               collect `(delete-file ,var)))))

