;;; Entrypoint for the command-line application

(in-package :leibowitz.cli)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals set at runtime

(defvar *data-directory*)
(defvar *cache-directory*)
(defvar *base-directory*)
(defvar *config-file*)
(defvar *library*)
(defvar *webserver*)
(defvar *interactive-session-p* T
  "This variable should be T when we're hooked up to a REPL, either while
hacking or running with a slynk server.  It determines how errors are
presented to the user; either by printing a message/returning an error
page or by promoting it in the hope that there is a debugger waiting.")
(defvar *load-config-p* T
  "Disables loading the config file for certain subcommands (help,
version) or when -q is passed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make me type less

(defmacro defsubcmd (name (cmd) (&rest definition) &body handler)
  (labels ((edit-sym (symbol-or-list &optional (fmt "~A"))
             (read-from-string (format NIL fmt symbol-or-list)))
           (sym2str (symbol)
             (string-downcase (format NIL "~A" symbol))))
    (let ((internal-name
            (etypecase name (symbol name) (list (edit-sym name "~{~A~^.~}"))))
          (external-name
            (sym2str (etypecase name (symbol name) (list (car (last name)))))))
      `(progn
         (defun ,(edit-sym internal-name "~A/definition") ()
           (clingon:make-command
            :pre-hook #'%subcommand-pre-hook
            :name ,external-name
            :handler (quote ,(edit-sym internal-name "~A/handler"))
            ,@definition))
         (defun ,(edit-sym internal-name "~A/handler") (,cmd)
           ,@handler)))))

(defmacro need-two-arguments
    ((arg1 arg2) (&key (else "Two arguments required!")) &body body)
  `(progn
     (unless (= (length (clingon:command-arguments cmd)) 2)
       (error ,else))
     (destructuring-bind (,arg1 ,arg2) (clingon:command-arguments cmd)
       ,@body)))

(defun %collect-stdin-lines ()
  (loop for line = (read-line *standard-input* nil 'eof)
        until (eq line 'eof)
        collect line))

(defun %subcommand-pre-hook (cmd)
  "This function is run before the subcommand handlers are invoked.  It
calls the handler function for the top-level arguments (which clingon
doesn't seem to call by default).  We're using those to configure very
fundamental aspects of our run time state so that's pretty important.
Once the top-level arguments have been processed and before the
relevant subcommand is run, it loads the config file."
  (let ((top-level-cmd (clingon:command-parent cmd)))
    (funcall (clingon:command-handler top-level-cmd) top-level-cmd))
  (when (and *load-config-p* (not (equal (clingon:command-name cmd) "help")))
    (load *config-file* :if-does-not-exist NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entrypoints

(defun main (&key (test-harness-p NIL) (test-argv NIL))
  (setf *base-directory* (user-homedir-pathname))
  (setf *data-directory* (uiop:xdg-data-home "leibowitz/"))
  (setf *cache-directory* (uiop:xdg-cache-home "leibowitz/"))
  (setf *config-file* (uiop:xdg-config-home "leibowitz/config.lisp"))
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
                       (untag/definition)
                       (untags/definition)
                       (mv/definition)
                       (cp/definition)
                       (rm/definition)
                       (ls/definition)
                       (show-tag/definition)
                       (mv-tag/definition)
                       (cp-tag/definition)
                       (rm-tag/definition)
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
                   :description "Print markdown usage docs to stdout."
                   :long-name "markdown-documentation"
                   :key :markdown-documentation)
                  (clingon:make-option
                   :flag
                   :description "Run a slynk server for interactive debugging."
                   :short-name #\s
                   :long-name "slynk"
                   :env-vars '("LEIBOWITZ_RUN_SLYNK")
                   :key :slynk)
                  (clingon:make-option
                   :integer
                   :description "Specify the port slynk will listen in."
                   :short-name #\p
                   :long-name "slynk-port"
                   :env-vars '("LEIBOWITZ_SLYNK_PORT")
                   :initial-value 4005
                   :key :slynk-port)
                  (clingon:make-option
                   :flag
                   :description "Wait for a slynk/swank client to connect before doing anything."
                   :short-name #\w
                   :long-name "slynk-wait"
                   :env-vars '("LEIBOWITZ_SLYNK_WAIT")
                   :key :slynk-wait)
                  (clingon:make-option
                   :filepath
                   :description "Specify an alternate config file."
                   :short-name #\c
                   :long-name "config"
                   :env-vars '("LEIBOWITZ_CONFIG")
                   :key :config)
                  (clingon:make-option
                   :flag
                   :description "Disable loading config file."
                   :short-name #\n
                   :long-name "no-config"
                   :key :no-config)
                  )))

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
      (setf *cache-directory* (merge-pathnames ".leibowitz/cache/" root))
      (setf *config-file* (merge-pathnames ".leibowitz/config.lisp" root))))
  (when (clingon:getopt cmd :slynk)
    (let ((slynk-port (clingon:getopt cmd :slynk-port))
          (slynk::*slynk-debug-p* NIL))
      (format T "Running debug server on localhost:~A...~%" slynk-port)
      (slynk:create-server :port slynk-port)
      (when (clingon:getopt cmd :slynk-wait)
        (format T "Waiting for a slynk/swank connection, press ENTER when ready.~%")
        (loop until (and (eql (read-char) #\Newline) slynk::*connections*)))))
  ;; FIXME: this will be inconvenient for hackers who use SLIME
  ;; instead of SLY!
  (unless slynk::*connections*
    (setf *interactive-session-p* NIL))
  (let ((config (clingon:getopt cmd :config)))
    (when config (setf *config-file* config)))
  (when (clingon:getopt cmd :no-config) (setf *load-config-p* NIL))
  (setf *library*
        (make-instance
         'sqlite-library
         :db-path (merge-pathnames "ontology.db" *data-directory*)
         :thumbnail-cache-dir (merge-pathnames "thumbnails/" *cache-directory*)
         :homedir *base-directory*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: help

(defsubcmd help (cmd)
    (:description "Another way to print help info."
     :usage "[subcommand]")
  (setf *load-config-p* NIL)
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
    (:description "Recursively index a file or files, reading from stdin if none are specified."
     :usage "[paths ...]")
  (let ((args (clingon:command-arguments cmd)))
    (if args
        (index *library* args :log T)
        (index *library* (%collect-stdin-lines) :log T))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: web

(defsubcmd web (cmd)
    (:description "Display a web UI."
     :usage "[-p|--port port]"
     :options (list (clingon:make-option
                     :integer
                     :description "Specify a port on which to run the web UI."
                     :short-name #\p
                     :long-name "port"
                     :env-vars '("LEIBOWITZ_WEB_PORT")
                     :initial-value 5000
                     :key :port)))
  (let ((port (clingon:getopt cmd :port)))
    (if *interactive-session-p*
        (setf hunchentoot:*catch-errors-p* NIL)
        (progn
          (setf hunchentoot:*catch-errors-p* T)
          (setf hunchentoot:*show-lisp-errors-p* T)
          (setf hunchentoot:*show-lisp-backtraces-p* T)))
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
     :usage "[-r|--reverse] [-s|--sort-by rank|modified|birth|accesses] [query terms...]"
     ;; FIXME: add support for tag filtering!
     :options (list (clingon:make-option
                     :flag
                     :description "Reverse sort direction, default shows best match at top."
                     :short-name #\r
                     :long-name "reverse"
                     :initial-value NIL
                     :key :reverse)
                    (clingon:make-option
                     :string
                     :description "Criterion to sort search results by, default is rank."
                     :short-name #\s
                     :long-name "sort-by"
                     :initial-value "rank"
                     :key :sort-by)))
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "Query string not specified"))
  (let* ((query (format NIL "~{~A~^ ~}" (clingon:command-arguments cmd)))
         (direction (if (clingon:getopt cmd :reverse) :descending :ascending))
         (sort-by (intern (string-upcase (clingon:getopt cmd :sort-by)) :keyword))
         (results (query *library* query :direction direction :sort-by sort-by)))
    (loop for res in results
          for id = (datum-id res)
          for tags = (datum-num-tags *library* id)
          do (format T "(~A tags) ~A~%" tags id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: show

(defsubcmd show (cmd)
    (:description "Print information about files."
     :usage "[paths...]")
  (loop for path in (clingon:command-arguments cmd)
        do (datum-print-long-report *library* (get-datum *library* path :error T))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: tag

(defsubcmd tag (cmd)
    (:description "Apply a tag to one or more files, read from stdin if none are
specified."
     :usage "[tag] [paths...]")
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No tag specified."))
  (let ((tag (car (clingon:command-arguments cmd)))
        (paths (if (= (length (clingon:command-arguments cmd)) 1)
                   (%collect-stdin-lines)
                   (cdr (clingon:command-arguments cmd)))))
    (loop for path in paths
          do (format T "Adding tag ~S to ~A~%" tag path)
             (add-datum-tags *library* path (list tag)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: tags

(defsubcmd tags (cmd)
    (:description "Apply one or more tags to a datum, reading from stdin if no tags are
specified."
     :usage "[path] [tags...]")
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No file specified."))
  (let ((path (car (clingon:command-arguments cmd)))
        (tags (if (= (length (clingon:command-arguments cmd)) 1)
                  (%collect-stdin-lines)
                  (cdr (clingon:command-arguments cmd)))))
    (format t "Adding tags ~S to datum ~A~%" tags path)
    (add-datum-tags *library* path tags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: untag

(defsubcmd untag (cmd)
    (:description "Remove a tag from one or more files, reading from stdin if none are
specified."
     :usage "[-c|--cascade] [tag] [paths...]"
     :options (list (clingon:make-option
                     :flag
                     :short-name #\c
                     :long-name "cascade"
                     :key :cascade
                     :description "Also remove tags from these data that depend on the supplied tag.")))
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No tag specified."))
  (let ((tag (car (clingon:command-arguments cmd)))
        (paths (if (= (length (clingon:command-arguments cmd)) 1)
                   (%collect-stdin-lines)
                   (cdr (clingon:command-arguments cmd)))))
  (loop for path in paths
        do (format T "Removing tag ~S from ~A~%" tag path)
           (del-datum-tags *library* path (list tag)
                           :cascade (clingon:getopt cmd :cascade)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: untags

(defsubcmd untags (cmd)
    (:description "Remove one or more tags from a file, reading tag names from stdin if
none are specified."
     :usage "[-c|--cascade] [path] [tags...]"
     :options (list (clingon:make-option
                     :flag
                     :short-name #\c
                     :long-name "cascade"
                     :key :cascade
                     :description "Also remove tags from this file that depend on the supplied tags.")))
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No path specified."))
  (let ((path (car (clingon:command-arguments cmd)))
        (tags (if (= (length (clingon:command-arguments cmd)) 1)
                  (%collect-stdin-lines)
                  (cdr (clingon:command-arguments cmd)))))
    (format T "Removing tags ~S from ~A~%" tags path)
    (del-datum-tags *library* path tags :cascade (clingon:getopt cmd :cascade))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: mv

(defsubcmd mv (cmd)
    (:description "Move or rename a file, keeping metadata and tags intact."
     :usage "[-f|--force] [src] [dst]"
     :options (list (clingon:make-option
                     :flag
                     :short-name #\f
                     :long-name "force"
                     :key :force
                     :description "Overwrite existing files.")))
  (need-two-arguments (src dst)
    (:else "You must specify a source name and a destination name")
    (format T "Moving datum ~A to ~A~%" src dst)
    (handler-case
        (move-datum *library* src dst :overwrite (clingon:getopt cmd :force))
      (datum-already-exists ()
        ;; Catch this error in order to print a more helpful message.
        (error "File ~S already exists, pass -f to overwrite.~%" dst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: cp

(defsubcmd cp (cmd)
    (:description "Copy a file, duplicating its tags and metadata."
     :usage "[-f|--force] [src] [dst]"
     :options (list (clingon:make-option
                     :flag
                     :short-name #\f
                     :long-name "force"
                     :key :force
                     :description "Overwrite new if it exists.")))
  (need-two-arguments (src dst)
      (:else "You must specify a source path and a destination path")
    (format T "Copying ~A to ~A~%" src dst)
    (handler-case
        (copy-datum *library* src dst :overwrite (clingon:getopt cmd :force))
      (datum-already-exists ()
        ;; Catch this error in order to print a more helpful message.
        (error "Destination ~A already exists, pass -f to overwrite.~%" dst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: rm

(defsubcmd rm (cmd)
    (:description "Remove a file, including all tag and metadata associations."
     :usage "[paths...]")
  ;; FIXME: rewrite this shitheap and move id resolution into core so
  ;; that we don't have to pass absolute paths to delete orphaned
  ;; files
  (let ((ids (loop for arg in (clingon:command-arguments cmd)
                   for path = (uiop:parse-unix-namestring arg)
                   collect (cond
                             ((probe-file path) (truename path))
                             ;; The forgoing branch failed, so it's
                             ;; not on disk.
                             ((get-datum *library* arg) arg)
                             ;; Raising this here rather than get an
                             ;; uninformative error by passing NIL to
                             ;; `del-datum'
                             (t (error 'no-such-datum-in-disk-or-db
                                       :lib *library* :id arg))))))

    (loop for id in ids
          do (format T "Removing ~A~%" id)
             (handler-case
                 (del-datum *library* id :error T)
               (datum-is-orphaned ())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: ls

(defsubcmd ls (cmd)
    (:description "List indexed files."
     :usage "[directory]")
  ;; FIXME: this function is very slow, we should record the number of
  ;; tags a datum has rather than fetching them all!
  ;; FIXME: this output is bland, ugly, uninformative, and doesn't
  ;; scale with terminal size.
  ;; FIXME: add support for listing multiple directories; infer cwd
  ;; when none
  (let* ((dir (if (clingon:command-arguments cmd)
                  (truename (car (clingon:command-arguments cmd)))
                  (uiop:getcwd))))
    (format T "SHOWING LISTING FOR ~A~%" dir)
    (loop for sub in (uiop:subdirectories dir)
          do (format T "~A~%"
                     (uiop:native-namestring (uiop:enough-pathname sub dir))))
    (loop for file in (library-list-files-in-dir *library* dir :include-unindexed T)
          do (etypecase file
               (datum (format T "(~A tags) ~A~%"
                              (datum-num-tags *library* file)
                              (uiop:native-namestring
                               (uiop:enough-pathname
                                (uiop:parse-unix-namestring (datum-id file))
                                dir))))
               (pathname (format T "UNINDEXED ~A~%"
                                 (uiop:native-namestring
                                  (uiop:enough-pathname file dir))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: show-tag

(defsubcmd show-tag (cmd)
    (:description "Show information about one or more tags."
     :usage "[tag names...]")
  (loop for name in (clingon:command-arguments cmd)
        do (tag-print-long-report *library* (get-tag *library* name :error T))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: ls-tags

(defsubcmd ls-tag (cmd)
    (:description "List all tags."
     :usage "")
  ;; FIXME: add support for different listing formats, etc
  (loop for tag in (list-tags *library*)
        do (format T "(~A data) ~A: ~S~%"
                   (tag-count tag) (tag-name tag) (tag-label tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: mv-tag

(defsubcmd mv-tag (cmd)
    (:description "Move or rename a tag."
     :usage "[-f|--force] [-m|--merge] [src] [dst]"
     :options (list (clingon:make-option
                     :flag
                     :short-name #\f
                     :long-name "force"
                     :key :force
                     :description "Overwrite dst if it already exists.")
                    (clingon:make-option
                     :flag
                     :short-name #\m
                     :long-name "merge"
                     :key :merge
                     :description "Merge src into dst if dst already exists.")))
  (need-two-arguments (src dst)
      (:else "You must specify a source name and a destination name")
    (format T "Moving tag ~A to ~A~%" src dst)
    (handler-case
        (move-tag *library* src dst :merge (clingon:getopt cmd :merge)
                                    :overwrite (clingon:getopt cmd :force))
      (tag-already-exists ()
        ;; Catch this error in order to print a more helpful message.
        (error "Tag ~S already exists, pass -f to overwrite or -m to merge~%" dst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: cp-tag

(defsubcmd cp-tag (cmd)
    (:description "Copy a tag."
     :usage "[-f|--force] [-m|--merge] [src] [dst]"
     :options (list (clingon:make-option
                     :flag
                     :short-name #\f
                     :long-name "force"
                     :key :force
                     :description "Overwrite dst if it already exists.")
                    (clingon:make-option
                     :flag
                     :short-name #\m
                     :long-name "merge"
                     :key :merge
                     :description "Merge src into dst if dst already exists.")))
  (need-two-arguments (src dst)
      (:else "You must specify a source name and a destination name")
    (format T "Copying tag ~A to ~A~%" src dst)
    (handler-case
        (copy-tag *library* src dst :merge (clingon:getopt cmd :merge)
                                    :overwrite (clingon:getopt cmd :force))
      (tag-already-exists ()
        ;; Catch this error in order to print a more helpful message.
        (error "Tag ~S already exists, pass -f to overwrite or -m to merge~%"
               dst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: rm-tag

(defsubcmd rm-tag (cmd)
    (:description "Remove a tag, leaving associated data intact."
     :usage "[tag names...]")
  ;; FIXME yuck 🙀
  (let ((names (loop for arg in (clingon:command-arguments cmd)
                     collect (if (get-tag *library* arg)
                                 arg
                                 ;; FIXME: `del-tag' should raise this
                                 ;; condition!
                                 (error 'no-such-tag :name arg)))))
    (loop for name in names
          for tag = (get-tag *library* name)
          do (format T "Removing tag ~S, leaving ~A data intact~%"
                     name (tag-count tag))
             (del-tag *library* name))))
