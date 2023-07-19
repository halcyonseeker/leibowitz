
(defpackage :thumbnailer
  (:use #:cl)
  (:export #:*thumbnail-cache-dir*
           #:get-thumbnail
           #:unsupported-file-type))

(in-package :thumbnailer)

(defparameter *ffmpeg-exe* "ffmpeg")
(defparameter *imagemagick-exe* "magick")
(defparameter *thumbnail-cache-dir*
  (ensure-directories-exist
   (pathname
    (format NIL "/tmp/thumbnailer_cache_dir-tmp~36R/" (random (expt 36 8))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entrypoint

(defun get-thumbnail (path mime)
  "Return a path to the thumbnail of PATH in `*thumbnail-cache-dir*',
generating it if it doesn't exist or if PATH was modified after the
thumbnail was last generated."
  (check-type path (or string pathname))
  (assert (probe-file path))
  (let* ((cached-path (get-cached-path path))
         (generate-thumbnail-p (or (not (probe-file cached-path))
                                   (> (file-write-date path)
                                      (file-write-date cached-path)))))
    (when generate-thumbnail-p
      (cond ((and (equal (subseq mime 0 6) "image/")
                  (not (equal mime "image/gif")))
             (imagemagick-generate-thumbnail path cached-path))
            ((equal (subseq mime 0 6) "video/")
             (ffmpeg-generate-thumbnail path cached-path))
            (T (restart-case
                   (error 'unsupported-file-type :mime mime :path path)
                 (skip-file () :report "Skip this file"
                   (return-from get-thumbnail (values NIL NIL)))))))
    (values cached-path generate-thumbnail-p)))

(define-condition unsupported-file-type (error)
  ((mime :initarg :mime)
   (path :initarg :path))
  (:report (lambda (c s)
             (with-slots (mime path) c
               (format s "Unsupported mime type ~S for file ~S"
                       mime path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generators

(defun imagemagick-generate-thumbnail (original-path cached-path)
  (uiop:run-program (list *imagemagick-exe*
                          (namestring original-path)
                          "-format" "jpg"
                          "-thumbnail" "300x300"
                          "-strip"
                          (namestring cached-path))))

(defun ffmpeg-generate-thumbnail (original-path cached-path)
  (uiop:run-program (list *ffmpeg-exe*
                          "-i" (namestring original-path)
                          "-vf" "select=eq(n\,34)"
                          "-vf" "scale=300:-2"
                          "-vframes" "1"
                          (namestring cached-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun get-cached-path (path)
  "Given an absolute path, return the path that would reference it in
the thumbnail cache."
  (let* ((path (if (uiop:absolute-pathname-p path)
                   path
                   (merge-pathnames path)))
         (path (namestring path))
         (path (pathname (subseq path 1 (length path))))
         (path (merge-pathnames path *thumbnail-cache-dir*)))
    (ensure-directories-exist (directory-namestring path))
    (pathname (concatenate 'string (namestring path) ".jpg"))))
