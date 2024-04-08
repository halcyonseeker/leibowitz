
(defpackage :thumbnailer
  (:use #:cl)
  (:export #:*thumbnail-cache-dir*
           #:get-thumbnail
           #:unsupported-file-type
           #:source-file-not-accessible
           #:thumbnail-creation-failed))

(in-package :thumbnailer)

(defparameter *ffmpeg-exe* "ffmpeg")
(defparameter *imagemagick-exe* "magick")
(defparameter *thumbnail-cache-dir*
  (ensure-directories-exist
   (merge-pathnames
    (pathname (format NIL "thumbnailer_cache_dir-tmp~36R/" (random (expt 36 8))))
    (uiop:temporary-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entrypoint

(defun get-thumbnail (path mime &key (async NIL))
  "Return a path to the thumbnail of PATH in `*thumbnail-cache-dir*',
generating it if it doesn't exist or if PATH was modified after the
thumbnail was last generated."
  (check-type path (or string pathname))
  (unless (probe-file path)
    (error 'source-file-not-accessible :path path))
  (let* ((cached-path (get-cached-path path))
         (generate-thumbnail-p (or (not (probe-file cached-path))
                                   (> (file-write-date path)
                                      (file-write-date cached-path)))))
    (when generate-thumbnail-p
      (restart-case
          (dispatch-thumbnailer path cached-path mime async)
        (skip-file () :report "Skip this file"
          (return-from get-thumbnail (values NIL NIL)))))
    (values cached-path generate-thumbnail-p)))

(define-condition unsupported-file-type (error)
  ((mime :initarg :mime)
   (path :initarg :path))
  (:report (lambda (c s)
             (with-slots (mime path) c
               (format s "Unsupported mime type ~S for file~%~S"
                       mime path)))))

(define-condition source-file-not-accessible (error)
  ((path :initarg :path))
  (:report (lambda (c s)
             (with-slots (path) c
               (format s "Failed to create thumbnail for ~S, file doesn't exist."
                       path)))))

(define-condition thumbnail-creation-failed (error)
  ((path :initarg :path)
   (mime :initarg :mime))
  (:report (lambda (c s)
             (with-slots (mime path) c
               (format s "Failed to create thumbnail for ~S file~%~S"
                       mime path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generators

(defun imagemagick-generate-thumbnail (original-path cached-path)
  (uiop:run-program (list *imagemagick-exe*
                          (namestring original-path)
                          "-format" "jpg"
                          "-thumbnail" "300x300"
                          "-strip"
                          (namestring cached-path))
                    :error-output T))

(defun imagemagick-generate-document-thumbnail (original-path cached-path)
  (uiop:run-program (list *imagemagick-exe*
                          "convert"
                          "-resize" "300x300"
                          (format NIL "~A[0]" (namestring original-path))
                          (namestring cached-path))))

(defun ffmpeg-generate-thumbnail (original-path cached-path)
  (uiop:run-program (list *ffmpeg-exe*
                          "-i" (namestring original-path)
                          "-vf" "select=eq(n\,34)"
                          "-vf" "scale=300:-2"
                          "-vframes" "1"
                          (namestring cached-path))
                    :error-output T))

(defun extract-epub-cover-and-resize (original-path cached-path)
  (let ((intermediate-manifest (uiop:tmpize-pathname
                                (merge-pathnames "thumbnailer_epub_manifest"
                                                 *thumbnail-cache-dir*)))
        (intermediate-cover (uiop:tmpize-pathname
                             (merge-pathnames "thumbnailer_epub_cover"
                                              *thumbnail-cache-dir*))))
    (unwind-protect
         ;; FIXME: What are the performance consequences of keeping a
         ;; zip file open for longer vs opening it twice?
         (zip:with-zipfile (zf original-path)
           (zip:zipfile-entry-contents (zip:get-zipfile-entry "content.opf" zf)
                                       intermediate-manifest)
           (destructuring-bind (cover-href cover-mime)
               (lquery:$1 (lquery:initialize intermediate-manifest)
                 "package" "manifest" "item"
                 (filter #'(lambda (item)
                             (equal (lquery-funcs:attr item :id) "cover")))
                 (combine (attr :href) (attr :media-type)))
             (zip:zipfile-entry-contents (zip:get-zipfile-entry cover-href zf)
                                         intermediate-cover)
             (dispatch-thumbnailer intermediate-cover cached-path cover-mime NIL)))
      (delete-file intermediate-manifest)
      (delete-file intermediate-cover))))

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

(defun dispatch-thumbnailer (path cached-path mime async)
  (let ((func (cond ((or (equal (subseq mime 0 6) "video/")
                         (and (equal (subseq mime 0 6) "audio/")
                              (not (equal mime "audio/flac")))
                         (equal mime "image/gif"))
                     #'ffmpeg-generate-thumbnail)
                    ((equal (subseq mime 0 6) "image/")
                     #'imagemagick-generate-thumbnail)
                    ((or (equal mime "application/pdf")
                         (equal mime "application/postscript")
                         ;; Microsoft Word
                         (if (libreoffice-available-p)
                             (mime-is-fancy-office-format mime)
                             NIL))
                     #'imagemagick-generate-document-thumbnail)
                    ((equal mime "application/epub+zip")
                     #'extract-epub-cover-and-resize)
                    (T (error 'unsupported-file-type :mime mime :path path)))))
    (handler-case
        (if async
            (bt:make-thread (lambda () (funcall func path cached-path))
                            :name "Thumbnailer worker")
            (funcall func path cached-path))
      (uiop:subprocess-error ()
        (error 'thumbnail-creation-failed :path path :mime mime)))))

(defun libreoffice-available-p ()
  "Check if libreoffice is available as imagemagick uses it under the
hood to convert office formats to PDFs."
  #-windows(handler-case
               (progn (uiop:run-program '("which" "libreoffice")) T)
             (uiop:subprocess-error () NIL))
  #+windows(error "thumbnailer doesn't support windows yet :("))

(defun mime-is-fancy-office-format (mime)
  (or
   (equal mime "application/msword")
   (equal mime "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
   ;; Microsoft Powerpoint
   (equal mime "application/vnd.ms-powerpoint")
   (equal mime "application/vnd.openxmlformats-officedocument.presentationml.presentation")
   ;; Libreoffice Writer
   (equal mime "application/vnd.oasis.opendocument.text")))
