;;; Entrypoint for the command-line application

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

(defun %collect-stdin-lines ()
  (loop for line = (read-line *standard-input* nil 'eof)
        until (eq line 'eof)
        collect line))

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
  (let ((src (car (clingon:command-arguments cmd)))
        (dst (cadr (clingon:command-arguments cmd))))
    ;; FIXME: this code smells like ugly edge cases 🙃
    (when (probe-file src) (setf src (namestring (truename src))))
    (when (probe-file dst) (setf dst (namestring (truename dst))))
    (format T "Moving ~A to ~A~%" src dst)
    (handler-case
        (move-datum *library* src dst :overwrite (clingon:getopt cmd :force))
      (datum-already-exists ()
        (format *error-output*
                "File ~S already exists on disk or in db, pass -f to overwrite.~%"
                dst)))))

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
    (let ((src (car (clingon:command-arguments cmd)))
          (dst (cadr (clingon:command-arguments cmd))))
      (when (probe-file src) (setf src (namestring (truename src))))
      (when (probe-file dst) (setf dst (namestring (truename dst))))
      (format T "Copying ~A to ~A~%" src dst)
      (handler-case
          (copy-datum *library* src dst :overwrite (clingon:getopt cmd :force))
        (datum-already-exists ()
          (format *error-output*
                  "File ~S already exists on disk or in db, pass -f to overwrite.~%"
                  dst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: rm

(defsubcmd rm (cmd)
    (:description "Remove a file, including all tag and metadata associations."
     :usage "[paths...]")
  ;; FIXME: rewrite this shitheap and move id resolution into core so
  ;; that we don't have to pass absolute paths to delete orphaned
  ;; files
  (let ((ids (loop for arg in (clingon:command-arguments cmd)
                   collect (cond
                             ((probe-file arg) (namestring (truename arg)))
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

;; FIXME: Given that leibowitz is ostensibly an enhanced filesystem,
;; it makes sense that we should have this functionality in the core.
;; Also fix /tree handler in routes.lisp.

(defsubcmd ls (cmd)
    (:description "List indexed files."
     :usage "[directory]")
  ;; FIXME: this function is very slow, we should record the number of
  ;; tags a datum has rather than fetching them all!
  ;; FIXME: this output is bland, ugly, uninformative, and doesn't
  ;; scale with terminal size.
  ;; FIXME: add support for listing multiple directories; infer cwd
  ;; when none
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
  (destructuring-bind (src dst) (clingon:command-arguments cmd)
    (move-tag *library* src dst
              :merge (clingon:getopt cmd :merge)
              :overwrite (clingon:getopt cmd :force))))

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
  (destructuring-bind (src dst) (clingon:command-arguments cmd)
    (copy-tag *library* src dst
              :merge (clingon:getopt cmd :merge)
              :overwrite (clingon:getopt cmd :force))))

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
