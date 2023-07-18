;;; Entrypoint for the command-line application

(defpackage :leibowitz-cli
  (:use #:cl
        #:leibowitz-core
        #:leibowitz-web)
  (:export #:main))

(in-package :leibowitz-cli)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals set at runtime

(defvar *library*)
(defvar *webserver*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entrypoints

(defun main ()
  ;; FIXME: clingon catches all condition and prints them to stderr.
  ;; They're often not very informative, figure out a way to bypass
  ;; this and handle them here, printing more friendly error messages
  ;; or stack traces as appropriate.
  (clingon:run (cli-definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top-level command-line

(defun cli-definition ()
  (clingon:make-command
   :name "leibowitz"
   :description "A lispy object-storage layer for unix file systems."
   :handler #'cli-handler
   :sub-commands (list (cli-subcommand/index-definition)
                       (cli-subcommand/web-definition)
                       (cli-subcommand/find-definition)
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
                   :long-name "generate-zsh-completions"
                   :key :generate-zsh-completions)
                  )))

(defun cli-handler (cmd)
  (when (clingon:getopt cmd :help)
    (clingon:print-usage-and-exit cmd *standard-output*))
  (when (clingon:getopt cmd :generate-zsh-completions)
    (clingon:print-documentation :zsh-completions (cli-definition) *standard-output*)
    (uiop:quit 0))
  (let ((root (clingon:getopt cmd :root)))
    (setf *library*
          (if root
              ;; FIXME: Normalize the path!  Is it relative?  Resolve it to
              ;; absolute.  Does it end in /, including just "/"?  Make sure
              ;; there aren't any "//" when we concatenate it.  Does it not
              ;; end in /?  We need a slash to access .leibowitz/.  Does it
              ;; start with a ~?  Resolve it to user-homedir-pathname.  What
              ;; about something bizarre like ~/../../mnt/some-disk?
              (make-instance 'sqlite-library
                             :db-path (format NIL "~A/.leibowitz/ontology.db" root)
                             :homedir root)
              (make-instance 'sqlite-library
                             :db-path (merge-pathnames "leibowitz/ontology.db"
                                                       (uiop:xdg-data-home))
                             :homedir (user-homedir-pathname))))))

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
;;; Subcommand: index

(defun cli-subcommand/index-definition ()
  (clingon:make-command
   :name "index"
   :description "Index a file, directory, or url into leibowitz."
   :usage "[options] [arguments ...]"
   :handler #'cli-subcommand/index-handler
   :options NIL))

(defun cli-subcommand/index-handler (cmd)
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

(defun cli-subcommand/web-definition ()
  (clingon:make-command
   :name "web"
   :description "Display a web UI."
   :usage "[-p port]"
   :handler #'cli-subcommand/web-handler
   :options (list (clingon:make-option
                   :integer
                   :description "Specify a port on which to run the web UI."
                   :short-name #\p
                   :long-name "port"
                   :env-vars '("LEIBOWITZ_WEB_PORT")
                   :initial-value 5000
                   :key :port))))

(defun cli-subcommand/web-handler (cmd)
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

(defun cli-subcommand/find-definition ()
  (clingon:make-command
   :name "find"
   :description "Search your data."
   :usage "[query terms...]"
   :handler #'cli-subcommand/find-handler
   ;; FIXME: once query supports it, here will go options to filter by
   ;; tags and other attributes
   ))

(defun cli-subcommand/find-handler (cmd)
  (handle-toplevel-args cmd)
  (let ((terms (format NIL "~{~A~^ ~}" (clingon:command-arguments cmd))))
    (loop for d in (query *library* terms)
          do (format T "~A~%" (datum-id d)))))
