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

(defun %get-toplevel-command (cmd)
  (let ((parent (clingon:command-parent cmd)))
    (if parent (%get-toplevel-command parent) cmd)))

(defun %subcommand-pre-hook (cmd)
  "This function is run before the subcommand handlers are invoked.  It
calls the handler function for the top-level arguments (which clingon
doesn't seem to call by default).  We're using those to configure very
fundamental aspects of our run time state so that's pretty important.
Once the top-level arguments have been processed and before the
relevant subcommand is run, it loads the config file."
  (let ((top-level-cmd (%get-toplevel-command cmd)))
    (funcall (clingon:command-handler top-level-cmd) top-level-cmd))
  (when (and *load-config-p* (not (equal (clingon:command-name cmd) "help")))
    (load *config-file* :if-does-not-exist NIL)))

(defun %print-help-for-subcommand (cmd)
  (let ((args (clingon:command-arguments cmd))
        (parent (clingon:command-parent cmd)))
    (labels ((find-subcmd (arg cmd)
               (find-if
                (lambda (c) (equal (clingon:command-name c) arg))
                (clingon:command-sub-commands (clingon:command-parent cmd)))))
      (if args
          (loop for arg in args
                for subcmd = (find-subcmd arg cmd)
                do (if subcmd
                       (clingon:print-usage-and-exit subcmd *standard-output*)
                       (error 'no-such-subcommand :subcmd arg)))
          (clingon:print-usage-and-exit parent *standard-output*)))))

(defun group-command-handler (cmd)
  "Handler for commands that are not intended to be called bare."
  (clingon:print-usage cmd *error-output*)
  (error "Error: you didn't pass a valid subcommand."))

(defun %edit-in-editor (&optional (initial-content "") (collect-lines-p T))
  (let ((initial-content (etypecase initial-content
                           (string initial-content)
                           (list (with-output-to-string (s)
                                   (format s "~{~A~%~}" initial-content)))))
        (final-content NIL)
        (path (uiop:tmpize-pathname #P"/tmp/leibowitz_editor_session"))
        (editor (uiop:getenv "EDITOR")))
    (format T "Invoking $EDITOR: \"~A ~A\"...~%" editor (namestring path))
    (when (or (null editor) (zerop (length editor)))
      (error "Set $EDITOR to a text editor program (eg, emacs) and try again."))
    (unless (interactive-stream-p *standard-output*)
      (error "Stdout doesn't appear to be connected to a terminal."))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output :if-exists :supersede)
             (format s "~A~%" initial-content))
           (uiop:run-program
            (format NIL "~A ~A" editor (uiop:unix-namestring path))
            :input :interactive :output :interactive :error-output T)
           (with-open-file (s path)
             (setf final-content (if collect-lines-p
                                     (collect-lines s)
                                     (read-stream-to-string s)))))
      (ignore-errors (delete-file path)))
    (format T "Returning from editor with ~A line~:P.~%" (length final-content))
    final-content))

(defun %collect-args-stdin-editor (cmd &optional (editor-preset NIL)
                                         (stdin-threshold 1))
  "For multiple subcommands we'd like to be able to read input from the
command line, from stdin, or pop open an editor so the user can do
certain batch operations more quickly.  Given a command implementing
the -e/--edit argument, a list of lines to present in an editor
window, and an offset into argv at which to look for important
arguments, this function returns a list of strings to be processed.
These strings were either retrieved from the command-line, read from
stdin, or interactively edited by the user at their text editor."
  (if (clingon:getopt cmd :edit)
      (%edit-in-editor editor-preset)
      (if (= (length (clingon:command-arguments cmd)) stdin-threshold)
          (collect-lines)
          (subseq (clingon:command-arguments cmd) stdin-threshold))))

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
                       (mv/definition)
                       (cp/definition)
                       (rm/definition)
                       (ls/definition)
                       (tag/definition)
                       )
   :options (list (clingon:make-option
                   :string
                   :description "Evaluate a Lisp form in the leibowitz package immediately after setup."
                   :short-name #\e
                   :long-name "eval"
                   :initial-value NIL
                   :key :eval)
                  (clingon:make-option
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
  (when (= (length (uiop:command-line-arguments)) 0)
    (group-command-handler cmd))
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
         :homedir *base-directory*))
  (let ((form (clingon:getopt cmd :eval))
        (*package* (find-package :leibowitz)))
    (when form
      (with-input-from-string (s form)
        (print (eval (read s)))
        (princ #\Newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: help

(defsubcmd help (cmd)
    (:description "Another way to print help info."
     :usage "[subcommand]")
  (setf *load-config-p* NIL)
  (%print-help-for-subcommand cmd))

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
        (index *library* (collect-lines) :log T))))

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
;;; Subcommand group: tag

;; Don't use defsubcmd since we don't want the :pre-hook ran until
;; right before tag subsubcommand handler... though I think that
;; doesn't make any difference with an empty handler
(defun tag/definition ()
  (clingon:make-command
   :name "tag"
   :handler #'group-command-handler
   :description "Parent command for all tag operations."
   :sub-commands (list (tag.help/definition)
                       ;; (tag-rm/definition)
                       (tag.show/definition)
                       (tag.ls/definition)
                       (tag.mv/definition)
                       (tag.cp/definition)
                       (tag.edit/definition)
                       )
   ))

;;;; Subcommand: tag help

(defsubcmd (tag help) (cmd)
    (:description "Print help for tag subcommands."
     :usage "[subcommand]")
  (setf *load-config-p* NIL)
  (%print-help-for-subcommand cmd))

;;;; Subcommand: tag show

(defsubcmd (tag show) (cmd)
    (:description "Show information about one or more tags."
     :usage "[tag names...]")
  (loop for name in (clingon:command-arguments cmd)
        do (tag-print-long-report *library* (get-tag *library* name :error T))))

;;;; Subcommand: tag ls

(defsubcmd (tag ls) (cmd)
    (:description "List tags, optionally filtered to union of specified paths."
     :usage "[-n|--no-label] [paths...]"
     :options (list (clingon:make-option
                     :flag
                     :short-name #\n
                     :long-name "no-label"
                     :key :no-label
                     :description "Don't print tag labels; useful for Unix scripting.")))
  (loop for tag in (alx:if-let ((paths (clingon:command-arguments cmd)))
                     ;; FIXME: filtering by files really, really
                     ;; should be done in list-tags
                     (sort (alx:flatten
                            (mapcar (lambda (p) (get-datum-tags *library* p))
                                    paths))
                           #'> :key (lambda (elt) (tag-count elt)))
                     (list-tags *library*))
        do (format T "~A file~:P~15T~A~%~:[~;  ~:*~S~%~]"
                   (tag-count tag) (tag-name tag)
                   (if (clingon:getopt cmd :no-label) NIL (tag-label tag)))))

;;;; Subcommand: tag mv

(defsubcmd (tag mv) (cmd)
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

;;;; Subcommand: tag cp

(defsubcmd (tag cp) (cmd)
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
;;; Subcommand group: tag edit

;;; FIXME: now add the -i/--invert option to switch from adding to
;;; removing!

(defun tag.edit/definition ()
  (clingon:make-command
   :name "edit"
   :handler #'group-command-handler
   :description "Edit tag files and parents, reading from stdin if arguments are omitted."
   :sub-commands (list (tag.edit.help/definition)
                       (tag.edit.tags/definition)
                       (tag.edit.files/definition)
                       (tag.edit.parents/definition)
                       (tag.edit.children/definition)
                       (tag.edit.label/definition)
                       )
   ))

;;;; Subcommand: tag edit help

(defsubcmd (tag edit help) (cmd)
    (:description "Print help for tag edit subcommands."
     :usage "[subcommand]")
  (setf *load-config-p* NIL)
  (%print-help-for-subcommand cmd))

;;; Subcommand: tag edit tags

(defsubcmd (tag edit tags) (cmd)
    (:description "Add (default) or remove multiple tags on a single file."
     :usage "[-r|--replace] [-e|--edit] [path] [tags...]"
     :options (list (clingon:make-option
                     :flag
                     :description "Replace this file's tags with the specified list."
                     :short-name #\r
                     :long-name "replace"
                     :initial-value NIL
                     :key :replace)
                    (clingon:make-option
                     :flag
                     :description "Edit the list of tags in $EDITOR, implies -r."
                     :short-name #\e
                     :long-name "edit"
                     :initial-value NIL
                     :key :edit)))
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No file specified."))
  (let* ((replace (or (clingon:getopt cmd :replace)
                      (clingon:getopt cmd :edit)))
         (path (car (clingon:command-arguments cmd)))
         (tags (%collect-args-stdin-editor
                cmd (mapcar #'tag-name (get-datum-tags *library* path)))))
    (if replace
        (format T "Replacing tags for file ~S with ~S~%" path tags)
        (format T "Adding tags to file ~S: ~S~%" path tags))
    (add-datum-tags *library* path tags :replace replace)))

;;;; Subcommand: tag edit files

(defsubcmd (tag edit files) (cmd)
    (:description "Add (default) or remove a single tag for multiple files"
     :usage "[-r|--replace] [-e|--edit] [tag] [files...]"
     :options (list (clingon:make-option
                     :flag
                     :description "Replace this tag's files with the specified list."
                     :short-name #\r
                     :long-name "replace"
                     :initial-value NIL
                     :key :replace)
                    (clingon:make-option
                     :flag
                     :description "Edit the list of files in $EDITOR, implies -r."
                     :short-name #\e
                     :long-name "edit"
                     :initial-value NIL
                     :key :edit)))
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No tag specified."))
  (let* ((replace (or (clingon:getopt cmd :replace)
                      (clingon:getopt cmd :edit)))
         (tag (car (clingon:command-arguments cmd)))
         (paths (mapcar (lambda (rel) (namestring (uiop:truenamize rel)))
                        (%collect-args-stdin-editor
                         cmd (mapcar #'datum-id
                                     (list-data *library* :tags (list tag)))))))
    (loop for path in paths
          do (format T "Adding tag ~S to file ~S~%" tag path)
             (add-datum-tags *library* path (list tag)))
    (when replace
      (loop for d in (list-data *library* :tags (list tag))
            for path = (datum-id d)
            unless (member path paths :test #'equal)
              do (format T "Dropping tag ~S from file ~S~%" tag path)
                 (del-datum-tags *library* path (list tag))))))

;;;; Subcommand: tag edit parents

(defsubcmd (tag edit parents) (cmd)
    (:description "Edit parent tags of a tag, default is to add to them."
     :usage "[-r|--replace] [-e|--edit] [tag] [parent tags...]"
     :options (list (clingon:make-option
                     :flag
                     :description "Replace this tag's parents with the specified list."
                     :short-name #\r
                     :long-name "replace"
                     :initial-value NIL
                     :key :replace)
                    (clingon:make-option
                     :flag
                     :description "Edit the list of parent tags in $EDITOR, implies -r"
                     :short-name #\e
                     :long-name "edit"
                     :initial-value NIL
                     :key :edit)))
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No tag specified."))
  (let* ((replace (or (clingon:getopt cmd :replace)
                      (clingon:getopt cmd :edit)))
         (tag (car (clingon:command-arguments cmd)))
         (parents (%collect-args-stdin-editor
                   cmd (mapcar #'tag-name (get-tag-predicates *library* tag)))))
    (loop for parent in parents
          do (format T "If a file has tag ~S, it now also has ~S~%"
                     tag parent)
             (add-tag-predicate *library* tag parent))
    (when replace
      (loop for p in (get-tag-predicates *library* tag)
            for name = (tag-name p)
            unless (member name parents :test #'equal)
              do (format T "Files tagged with ~S, will no longer have ~S~%"
                         tag name)
                 (del-tag-predicate *library* tag name)))))

;;;; Subcommand: tag edit children

(defsubcmd (tag edit children) (cmd)
    (:description "Edit a tag's children, default is to add."
     :usage "[-r|--replace] [-e|--edit] [tag] [child tags...]"
     :options (list (clingon:make-option
                     :flag
                     :description "Replace this tag's children with the specified list."
                     :short-name #\r
                     :long-name "replace"
                     :initial-value NIL
                     :key :replace)
                    (clingon:make-option
                     :flag
                     :description "Edit the list of child tags in $EDITOR, implies -r."
                     :short-name #\e
                     :long-name "edit"
                     :initial-value NIL
                     :key :edit)))
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No tag specified."))
  (let* ((replace (or (clingon:getopt cmd :replace)
                      (clingon:getopt cmd :edit)))
         (tag (car (clingon:command-arguments cmd)))
         (children (%collect-args-stdin-editor
                    cmd (mapcar #'tag-name (get-tag-predicands *library* tag)))))
    (loop for child in children
          do (format T "Tag ~S will now be added to files with tag ~S~%" tag child)
             (add-tag-predicate *library* child tag))
    (when replace
      (loop for c in (get-tag-predicands *library* tag)
            for name = (tag-name c)
            unless (member name children :test #'equal)
              do (format T "Tag ~S will no longer be applied to files with tag ~S~%"
                         tag name)
                 (del-tag-predicate *library* name tag)))))

;;;; Subcommand: tag edit label

(defsubcmd (tag edit label) (cmd)
    (:description "Edit a tag's label, replacing it if it already exists"
     :usage "[-e|--edit] [tag] [label]"
     :options (list (clingon:make-option
                     :flag
                     :description "Edit the label in $EDITOR."
                     :short-name #\e
                     :long-name "edit"
                     :initial-value NIL
                     :key :edit)))
  (when (zerop (length (clingon:command-arguments cmd)))
    (error "No tag specified."))
  (let* ((name (car (clingon:command-arguments cmd)))
         (tag  (get-tag *library* name))
         (label (if (clingon:getopt cmd :edit)
                    (%edit-in-editor (if tag (tag-label tag) "") NIL)
                    (if (= (length (clingon:command-arguments cmd)) 1)
                        (read-stream-to-string)
                        (cadr (clingon:command-arguments cmd))))))
    (unless tag (setf tag (make-instance 'tag :name name)))
    (when (zerop (length label)) (setf label NIL))
    (format T "Label: ~S~%" label)
    (setf (tag-label tag) label)
    (add-tag *library* tag)))
