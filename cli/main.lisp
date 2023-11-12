;;; Entrypoint for the command-line application

(defpackage :leibowitz.cli
  (:use #:cl
        #:leibowitz.core
        #:leibowitz.web)
  (:export #:main)
  (:export #:*data-directory*
           #:*cache-directory*
           #:*base-directory*
           #:*library*
           #:*webserver*))

(in-package :leibowitz.cli)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals set at runtime

(defvar *data-directory*)
(defvar *cache-directory*)
(defvar *base-directory*)
(defvar *library*)
(defvar *webserver*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make me type less

(defmacro defsubcmd (name (cmd) (&rest definition) &body handler)
  `(progn
     (defun ,(read-from-string (format NIL "~A/definition" name)) ()
       (clingon:make-command
        :name ,(string-downcase (format NIL "~A" name))
        :handler (quote ,(read-from-string (format NIL "~A/handler" name)))
        ,@definition))
     (defun ,(read-from-string (format NIL "~A/handler" name)) (,cmd)
       (handle-toplevel-args ,cmd)
       ,@handler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entrypoints

(defun main (&key (test-harness-p NIL) (test-argv NIL))
  (setf *base-directory* (user-homedir-pathname))
  (setf *data-directory* (uiop:xdg-data-home "leibowitz/"))
  (setf *cache-directory* (uiop:xdg-cache-home "leibowitz/"))
  ;; FIXME: clingon catches all condition and prints them to stderr.
  ;; They're often not very informative, figure out a way to bypass
  ;; this and handle them here, printing more friendly error messages
  ;; or stack traces as appropriate.
  (if test-harness-p
      (let ((cmd (clingon:parse-command-line (toplevel/definition) test-argv)))
        (funcall (clingon:command-handler cmd) cmd))
      (clingon:run (toplevel/definition))))

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
                       (mv/definition)
                       (rm/definition)
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

(defsubcmd help (cmd)
    (:description "Another way to print help info."
     :usage "[subcommand]")
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

(defsubcmd info (cmd)
    (:description "Print information and statistics about the dataset."
     :usage "")
  (format T "Base directory:  ~A~%" (namestring *base-directory*))
  (format T "Data directory:  ~A~%" (namestring *data-directory*))
  (format T "Cache directory: ~A~%" (namestring *cache-directory*))
  (library-print-info *library*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: index

(defsubcmd index (cmd)
    (:description "Index a file, directory, or url into leibowitz."
     :usage "[options] [arguments ...]")
  (let* ((args (clingon:command-arguments cmd))
         (root (clingon:getopt cmd :root))
         (jobs (if args args (if root (list root) (error "Idk what to index bro")))))
    (loop for job in jobs
          do (format T "Indexing ~A..." job)
             (finish-output)
             (index *library* (truename job))
             (format T "done~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: web

(defsubcmd web (cmd)
    (:description "Display a web UI."
     :usage "[-p port]"
     :options (list (clingon:make-option
                     :integer
                     :description "Specify a port on which to run the web UI."
                     :short-name #\p
                     :long-name "port"
                     :env-vars '("LEIBOWITZ_WEB_PORT")
                     :initial-value 5000
                     :key :port)))
  (let ((port (clingon:getopt cmd :port)))
    (format T "Running webserver on localhost:~A...~%" port)
    (setf *webserver* (make-instance 'webserver :port port :library *library*))
    (webserver-run *webserver*)
    (bt:join-thread
     (find-if (lambda (th) (search "hunchentoot-listener-" (bt:thread-name th)))
              (bt:all-threads)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: find

(defsubcmd find (cmd)
    (:description "Search your data."
     :usage "[query terms...]"
     ;; FIXME: once query supports it, here will go options to filter by
     ;; tags and other attributes
     )
  (let ((terms (format NIL "~{~A~^ ~}" (clingon:command-arguments cmd))))
    (loop for d in (query *library* terms)
          do (format T "~A~%" (datum-id d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: show

(defsubcmd show (cmd)
    (:description "Print summaries of data."
     :usage "[data ids...]")
  (loop for p in (clingon:command-arguments cmd)
        do (if (probe-file p)
               (datum-print-long-report
                *library* (get-datum *library* (truename p)))
               (error 'datum-not-indexed :lib *library* :id p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: tag

(defsubcmd tag (cmd)
    (:description "Apply a tag to one or more data."
     :usage "[tag] [data ids...]")
  ;; FIXME: this will spit errors when working with URLS, but honestly
  ;; I'm not so sure that URLs should be supported as data ids along
  ;; with file names.  Perhaps "source url" could instead be a
  ;; metadata tag for files in bookmark or torrent collections...
  (let ((tag (car (clingon:command-arguments cmd)))
        (data (mapcar #'truename (cdr (clingon:command-arguments cmd)))))
    (loop for d in data
          do (format T "Adding tag ~S to datum ~S~%" tag d)
             (add-datum-tags *library* d (list tag)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: tags

(defsubcmd tags (cmd)
    (:description "Apply one or more tags to a datum."
     :usage "[datum id] [tags...]")
  (let ((id (truename (car (clingon:command-arguments cmd))))
        (tags (cdr (clingon:command-arguments cmd))))
    (format T "Adding tags ~S to datum ~S~%" tags id)
    (add-datum-tags *library* id tags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: mv

(defsubcmd mv (cmd)
    (:description "Move or rename a datum, keeping metadata and tags intact."
     :usage "[old] [new]"
     :options (list (clingon:make-option
                     :boolean
                     :short-name #\f
                     :long-name "force"
                     :key :force
                     :description "Overwrite existing files.")))
  (let ((src (car (clingon:command-arguments cmd)))
        (dst (cadr (clingon:command-arguments cmd))))
    (when (probe-file src) (setf src (truename src)))
    (when (probe-file dst) (setf dst (truename dst)))
    (format T "Moving ~A to ~A~%" src dst)
    (format T "Overwrite? ~S~%" (clingon:getopt cmd :force))
    (handler-case
        (move-datum *library* src dst :overwrite (clingon:getopt cmd :force))
      (datum-already-exists ()
        (format *error-output*
                "File ~S already exists on disk or in db, pass -f to overwrite.~%"
                dst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: rm

(defsubcmd rm (cmd)
    (:description "Remove a datum, including all tag and metadata associations."
     :usage "[datum ids...]")
  (let ((ids (loop for arg in (clingon:command-arguments cmd)
                   collect (cond
                             ;; If it exists on disk we act like rm(1)
                             ;; regardless of whether it's in the
                             ;; database.  FIXME: gate this behind a
                             ;; flag?
                             ((probe-file arg) (namestring (truename arg)))
                             ;; The forgoing branch failed, so it's
                             ;; not on disk.
                             ((get-datum *library* arg) arg)
                             ;; FIXME: `del-datum' should raise this
                             ;; condition!  How to do so while
                             ;; retaining idempotency?
                             (t (error 'no-such-datum-in-disk-or-db
                                       :lib *library* :id arg))))))
    (loop for id in ids
          do (format T "Removing ~A~%" id)
             (del-datum *library* id)
             ;; FIXME: `del-datum' doesn't do this, should it?
             ;; `move-datum' and `copy-datum' do act on the disk, but
             ;; should they?  Should we gate the behavior behind a
             ;; keyword argument?  Should we still consider the idea
             ;; that there will by subclasses of `datum' like
             ;; `datum-link' whose ID isn't a path?  In any case,
             ;; deleting/moving/copying files should go through the
             ;; collections API, mediating disk access is what it's
             ;; for!
             (when (probe-file id)
               (delete-file id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: ls

;; FIXME: Given that leibowitz is ostensibly an enhanced filesystem,
;; it makes sense that we should have this functionality in the core.
;; Also fix /tree handler in routes.lisp.

(defsubcmd ls (cmd)
    (:description "List indexed data."
     :usage "[directory]")
  ;; FIXME: this function is very slow, we should record the number of
  ;; tags a datum has rather than fetching them all!
  ;; FIXME: this output is bland, ugly, uninformative, and doesn't
  ;; scale with terminal size.
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

(defsubcmd show-tag (cmd)
    (:description "Show information about a tag."
     :usage "[tag names....]")
  (loop for tag in (clingon:command-arguments cmd)
        do (tag-print-long-report *library* (get-tag *library* tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: ls-tags

(defsubcmd ls-tag (cmd)
    (:description "List all tags."
     :usage "")
  (loop for tag in (list-tags *library*)
        do (format T "(~A data) ~A: ~S~%"
                   (tag-count tag) (tag-name tag) (tag-label tag))))
