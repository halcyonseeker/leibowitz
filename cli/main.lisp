;;; Entrypoint for the command-line application

(defpackage :leibowitz.cli
  (:use #:cl
        #:leibowitz.core
        #:leibowitz.web)
  (:export #:main))

(in-package :leibowitz.cli)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals set at runtime

(defvar *data-directory*)
(defvar *cache-directory*)
(defvar *base-directory*)
(defvar *library*)
(defvar *webserver*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entrypoints

(defun main ()
  (setf *base-directory* (user-homedir-pathname))
  (setf *data-directory* (uiop:xdg-data-home "leibowitz/"))
  (setf *cache-directory* (uiop:xdg-cache-home "leibowitz/"))
  ;; FIXME: clingon catches all condition and prints them to stderr.
  ;; They're often not very informative, figure out a way to bypass
  ;; this and handle them here, printing more friendly error messages
  ;; or stack traces as appropriate.
  (clingon:run (toplevel/definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top-level command-line

(defun toplevel/definition ()
  (clingon:make-command
   :name "leibowitz"
   :description "A lispy object-storage layer for unix file systems."
   :handler #'toplevel/handler
   :sub-commands (list (help/definition)
                       (info/definition)
                       (index/definition)
                       (web/definition)
                       (find/definition)
                       (show/definition)
                       (tag/definition)
                       (tags/definition)
                       (ls/definition)
                       (show-tag/definition)
                       (ls-tag/definition)
                       )
   :options (list (clingon:make-option
                   :filepath
                   :description "Specify a directory in which to run in root mode."
                   :short-name #\r
                   :long-name "root"
                   :env-vars '("LEIBOWITZ_ROOT")
                   :initial-value NIL
                   :key :root)
                  (clingon:make-option
                   :flag
                   :description "Print zsh-completions to stdout."
                   :long-name "zsh-completions"
                   :key :zsh-completions)
                  (clingon:make-option
                   :flag
                   :description "Print markdown usage docs to standard-output."
                   :long-name "markdown-documentation"
                   :key :markdown-documentation))))

(defun toplevel/handler (cmd)
  (when (clingon:getopt cmd :help)
    (clingon:print-usage-and-exit cmd *standard-output*))
  (when (clingon:getopt cmd :zsh-completions)
    (clingon:print-documentation :zsh-completions (toplevel/definition) *standard-output*)
    (uiop:quit 0))
  (when (clingon:getopt cmd :markdown-documentation)
    (clingon:print-documentation :markdown (toplevel/definition) T)
    (uiop:quit 0))
  (let ((root (clingon:getopt cmd :root)))
    (when root
      (setf root (truename root))
      (setf *base-directory* root)
      (setf *data-directory* (merge-pathnames ".leibowitz/" root))
      (setf *cache-directory* (merge-pathnames ".leibowitz/cache/" root))))
  (setf *library*
        (make-instance
         'sqlite-library
         :db-path (merge-pathnames "ontology.db" *data-directory*)
         :thumbnail-cache-dir (merge-pathnames "thumbnails/" *cache-directory*)
         :homedir *base-directory*)))

(defun handle-toplevel-args (cmd)
  "By default clingon doesn't call the handler function for the
top-level command when a subcommand is invoked.  Since we're using the
top-level arguments to configure application state, we need to process
them before running any subcommand handler.  The first thing all
subcommand handlers should do is call this function on their
argument."
  (let ((top-level-cmd (clingon:command-parent cmd)))
    (funcall (clingon:command-handler top-level-cmd) top-level-cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: help

(defun help/definition ()
  (clingon:make-command
   :name "help"
   :description "Another way to print help info."
   :usage "[subcommand]"
   :handler #'help/handler
   :options NIL))

(defun help/handler (cmd)
  (handle-toplevel-args cmd)
  (let ((args (clingon:command-arguments cmd)))
    (if args
        (let ((subcmd-to-print (find-if (lambda (cmd)
                                          (equal (clingon:command-name cmd)
                                                 (car args)))
                                        (clingon:command-sub-commands
                                         (clingon:command-parent cmd)))))
          (if subcmd-to-print
              (clingon:print-usage-and-exit subcmd-to-print
                                            *standard-output*)
              ;; FIXME: we need to detect invalid subcommands in
              ;; general and dumb help to stderr, exiting with nonzero
              (error "No such subcommand")))
        (clingon:print-usage-and-exit (clingon:command-parent cmd)
                                      *standard-output*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: info

(defun info/definition ()
  (clingon:make-command
   :name "info"
   :description "Print information and statistics about the dataset."
   :usage ""
   :handler #'info/handler
   :options NIL))

(defun info/handler (cmd)
  (handle-toplevel-args cmd)
  (format T "Base directory:  ~A~%" (namestring *base-directory*))
  (format T "Data directory:  ~A~%" (namestring *data-directory*))
  (format T "Cache directory: ~A~%" (namestring *cache-directory*))
  (library-print-info *library*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: index

(defun index/definition ()
  (clingon:make-command
   :name "index"
   :description "Index a file, directory, or url into leibowitz."
   :usage "[options] [arguments ...]"
   :handler #'index/handler
   :options NIL))

(defun index/handler (cmd)
  (handle-toplevel-args cmd)
  (let* ((args (clingon:command-arguments cmd))
         (root (clingon:getopt cmd :root))
         (jobs (if args args (if root (list root) (error "Idk what to index bro")))))
    (loop for job in jobs
          do (format T "Indexing ~A..." job)
             (finish-output)
             (index *library* job)
             (format T "done~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: web

(defun web/definition ()
  (clingon:make-command
   :name "web"
   :description "Display a web UI."
   :usage "[-p port]"
   :handler #'web/handler
   :options (list (clingon:make-option
                   :integer
                   :description "Specify a port on which to run the web UI."
                   :short-name #\p
                   :long-name "port"
                   :env-vars '("LEIBOWITZ_WEB_PORT")
                   :initial-value 5000
                   :key :port))))

(defun web/handler (cmd)
  (handle-toplevel-args cmd)
  (let ((port (clingon:getopt cmd :port)))
    (format T "Running webserver on localhost:~A...~%" port)
    (setf *webserver* (make-instance 'webserver :port port :library *library*))
    (webserver-run *webserver*)
    (bt:join-thread
     (find-if (lambda (th) (search "hunchentoot-listener-" (bt:thread-name th)))
              (bt:all-threads)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: find

(defun find/definition ()
  (clingon:make-command
   :name "find"
   :description "Search your data."
   :usage "[query terms...]"
   :handler #'find/handler
   ;; FIXME: once query supports it, here will go options to filter by
   ;; tags and other attributes
   ))

(defun find/handler (cmd)
  (handle-toplevel-args cmd)
  (let ((terms (format NIL "~{~A~^ ~}" (clingon:command-arguments cmd))))
    (loop for d in (query *library* terms)
          do (format T "~A~%" (datum-id d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: show

(defun show/definition ()
  (clingon:make-command
   :name "show"
   :description "Print summaries of data."
   :usage "[data ids...]"
   :handler #'show/handler))

(defun show/handler (cmd)
  (handle-toplevel-args cmd)
  (loop for p in (clingon:command-arguments cmd)
        do (datum-print-long-report
            *library* (get-datum *library* (truename p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: tag

(defun tag/definition ()
  (clingon:make-command
   :name "tag"
   :description "Apply a tag to one or more data."
   :usage "[tag] [data ids...]"
   :handler #'tag/handler))

;;; FIXME: this will spit errors when working with URLS, but honestly
;;; I'm not so sure that URLs should be supported as data ids along
;;; with file names.  Perhaps "source url" could instead be a metadata
;;; tag for files in bookmark or torrent collections...
(defun tag/handler (cmd)
  (handle-toplevel-args cmd)
  (let ((tag (car (clingon:command-arguments cmd)))
        (data (mapcar #'truename (cdr (clingon:command-arguments cmd)))))
    (loop for d in data
          do (format T "Adding tag ~S to datum ~S~%" tag d)
             (add-datum-tags *library* d (list tag)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: tags

(defun tags/definition ()
  (clingon:make-command
   :name "tags"
   :description "Apply one or more tags to a datum."
   :usage "[datum id] [tags...]"
   :handler #'tags/handler))

(defun tags/handler (cmd)
  (handle-toplevel-args cmd)
  (let ((id (truename (car (clingon:command-arguments cmd))))
        (tags (cdr (clingon:command-arguments cmd))))
    (format T "Adding tags ~S to datum ~S~%" tags id)
    (add-datum-tags *library* id tags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: ls

;; FIXME: Given that leibowitz is ostensibly an enhanced filesystem,
;; it makes sense that we should have this functionality in the core.
;; Also fix /tree handler in routes.lisp.

(defun ls/definition ()
  (clingon:make-command
   :name "ls"
   :description "List indexed data."
   :usage "[directory]"
   :handler #'ls/handler))

;; FIXME: this function is very slow, we should record the number of
;; tags a datum has rather than fetching them all!
;; FIXME: this output is bland, ugly, uninformative, and doesn't scale
;; with terminal size.
(defun ls/handler (cmd)
  (handle-toplevel-args cmd)
  (let* ((dir (car (clingon:command-arguments cmd))))
    (loop for path in (reverse (uiop:directory-files
                                (truename
                                 (if dir dir (uiop:getcwd)))))
          for datum = (get-datum *library* path)
          when datum
            do (format T "(~A tags) ~A~%"
                       (length (get-datum-tags *library* datum))
                       (path:basename (datum-id datum))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: show-tag

(defun show-tag/definition ()
  (clingon:make-command
   :name "show-tag"
   :description "Show information about a tag."
   :usage "[tag names....]"
   :handler #'show-tag/handler))

(defun show-tag/handler (cmd)
  (handle-toplevel-args cmd)
  (loop for tag in (clingon:command-arguments cmd)
        do (tag-print-long-report *library* (get-tag *library* tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: ls-tags

(defun ls-tag/definition ()
  (clingon:make-command
   :name "ls-tags"
   :description "List all tags."
   :usage ""
   :handler #'ls-tag/handler))

(defun ls-tag/handler (cmd)
  (handle-toplevel-args cmd)
  (loop for tag in (list-tags *library*)
        do (format T "(~A data) ~A: ~S~%"
                   (tag-count tag) (tag-name tag) (tag-label tag))))
