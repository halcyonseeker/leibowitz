
(defpackage :thumbnailer
  (:use #:cl)
  (:export #:*thumbnail-cache-dir*
           #:*mutool-exe*
           #:*ffmpeg-exe*
           #:*imagemagick-exe*
           #:get-thumbnail
           #:unsupported-file-type
           #:source-file-not-accessible
           #:thumbnail-creation-failed))

(in-package :thumbnailer)

(defparameter *mutool-exe* "mutool")
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
  (setf path (etypecase path
               (string (uiop:parse-native-namestring path))
               (pathname path)))
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
               (format s "Unsupported mime type ~S for file~%  Path: ~S"
                       mime path)))))

(define-condition source-file-not-accessible (error)
  ((path :initarg :path))
  (:report (lambda (c s)
             (with-slots (path) c
               (format s "Failed to create thumbnail for ~S, file doesn't exist."
                       path)))))

(define-condition thumbnail-creation-failed (error)
  ((path :initarg :path)
   (mime :initarg :mime)
   (why  :initarg :why))
  (:report (lambda (c s)
             (with-slots (mime path why) c
               (format s "Failed to create thumbnail for ~S file~%  Path: ~S~%  Reason: ~A"
                       mime path why)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generators

(defun imagemagick-generate-thumbnail (original-path cached-path)
  (uiop:run-program (list *imagemagick-exe*
                          (uiop:native-namestring original-path)
                          "-format" "jpg"
                          "-thumbnail" "300x300"
                          "-strip"
                          (uiop:native-namestring cached-path))
                    :error-output T))

(defun imagemagick-generate-document-thumbnail (original-path cached-path)
  (uiop:run-program (list *imagemagick-exe*
                          "convert"
                          "-resize" "300x300"
                          (format NIL "~A[0]" (uiop:native-namestring original-path))
                          (uiop:native-namestring cached-path))))

(defun ffmpeg-generate-thumbnail (original-path cached-path)
  (uiop:run-program (list *ffmpeg-exe*
                          "-i" (uiop:native-namestring original-path)
                          "-vf" "select=eq(n\,34)"
                          "-vf" "scale=300:-2"
                          "-vframes" "1"
                          (uiop:native-namestring cached-path))
                    :error-output T))

(defun mupdf-generate-thumbnail (original-path cached-path)
  (uiop:run-program (list *mutool-exe*
                          "draw"
                          "-q"
                          "-w" "300"
                          "-h" "300"
                          "-o" (uiop:native-namestring cached-path)
                          (uiop:native-namestring original-path)
                          "1")
                    :error-output T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun get-cached-path (path)
  "Given an absolute path, return the path that would reference it in
the thumbnail cache."
  (let* ((path (if (uiop:absolute-pathname-p path)
                   path
                   (merge-pathnames path)))
         (path (uiop:native-namestring path))
         (path (pathname (subseq path 1 (length path))))
         (path (merge-pathnames path *thumbnail-cache-dir*)))
    (ensure-directories-exist (directory-namestring path))
    (pathname (concatenate 'string (uiop:native-namestring path) ".jpg"))))

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
                         (if (libreoffice-available-p)
                             (mime-is-fancy-office-format mime)
                             NIL))
                     #'imagemagick-generate-document-thumbnail)
                    ((or (equal mime "application/epub+zip")
                         (equal mime "application/vnd.comicbook+zip")
                         (equal mime "application/fictionbook2+zip")
                         (equal mime "application/fictionbook3+zip")
                         (equal mime "application/x-mobipocket-ebook")
                         (equal mime "application/vnd.ms-xpsdocument")
                         (equal mime "application/oxps"))
                     #'mupdf-generate-thumbnail)
                    (T (error 'unsupported-file-type :mime mime :path path)))))
    (handler-case
        (if async
            (bt:make-thread (lambda () (funcall func path cached-path))
                            :name "Thumbnailer worker")
            (funcall func path cached-path))
      (uiop:subprocess-error (c)
        (error 'thumbnail-creation-failed :path path :mime mime
                                          :why (format NIL "~S" c))))))

(defun libreoffice-available-p ()
  "Check if libreoffice is available as imagemagick uses it under the
hood to convert office formats to PDFs."
  #-windows(handler-case
               (progn (uiop:run-program '("which" "libreoffice")) T)
             (uiop:subprocess-error () NIL))
  #+windows(error "thumbnailer doesn't support windows yet :("))

(defun mime-is-fancy-office-format (mime)
  (or
   ;; Microsoft Word
   (equal mime "application/msword")
   (equal mime "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
   ;; Microsoft Powerpoint
   (equal mime "application/vnd.ms-powerpoint")
   (equal mime "application/vnd.openxmlformats-officedocument.presentationml.presentation")
   ;; Libreoffice Writer
   (equal mime "application/vnd.oasis.opendocument.text")))
