;;; Datum subclasses for files with the mime type text/*

(in-package :leibowitz-core)

;; FIXME: for each of these, figure out an absolute path-resolution
;; algorithm for datum-id.  Maybe add a datum slot storing the path to
;; the collection that manages it?

(defclass datum-text (datum)
  ()
  (:documentation "Datum implementation for files with the mimetype text/*"))

(defmethod %datum-find-terms ((d datum-text))
  "Return the contents of this file to be indexed by the search engine."
  (setf (datum-terms d)
        (with-open-file (in (datum-id d))
          (with-output-to-string (out)
            (loop for line = (read-line in nil 'eof)
                  until (eq line 'eof)
                  do (format out "~A~%" line))))))

(defmethod datum-html-report ((l library) (d datum-text))
  (declare (ignore l))
  `((:section
     (:pre ,(cl-who:escape-string
             (with-open-file (stream (datum-id d))
               (let ((contents (make-string (file-length stream))))
                 (read-sequence contents stream)
                 contents)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass datum-text/html (datum)
  ()
  (:documentation "Datum implementation to extract text from HTML.  I'm creating this
right now just for testing purposes, it will be expanded with a proper
HTML parser later."))

(defmethod %datum-find-terms ((d datum-text/html))
  "This is just here to pass the tests of `datum''s class-changing
behavior.  It is obviously wrong."
  "hi :^3")
