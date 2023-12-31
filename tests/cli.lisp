;;; Integration tests for the command line

(in-package :leibowitz/tests)

(define-test cli)

(defmacro define-cli-test (name (main) &body body)
  (check-type main symbol)
  `(define-test ,name :parent cli
     (let (;; NOTE: each of these variables should be unbound at
           ;; startup, however capturing them like this binds them to
           ;; NIL.
           *data-directory*
           *cache-directory*
           *base-directory*
           *library*
           *webserver*
           (*default-pathname-defaults*
             (ensure-directories-exist
              (pathname
               (format NIL "/tmp/leibowitz_cli_test_cwd-tmp~36R/"
                       (random (expt 36 8))))))
           (home (user-homedir-pathname))
           (config (uiop:xdg-config-home))
           (cache (uiop:xdg-cache-home))
           (data (uiop:xdg-data-home)))
       (labels ((,main (&rest cli)
                  (leibowitz.cli:main :test-harness-p T :test-argv cli))
                (setenv (key val)
                  (nix:setenv key (namestring val))))
         ;; Set this process's copy of $HOME to the new value of
         ;; `*default-pathname-defaults*' for the duration of each
         ;; test case.  $XDG_CONFIG_HOME, $XDG_CACHE_HOME, and
         ;; $XDG_DATA_HOME are set as well to the appropriate
         ;; subdirectories thereof.  This fools the likes of
         ;; `user-homedir-pathname' and `uiop:xdg-*-home' into
         ;; thinking that we're running as normal in the user's home
         ;; directory for the duration of these tests, allowing us to
         ;; screw around without stepping on the their toes.
         (setenv "HOME" *default-pathname-defaults*)
         (setenv "XDG_CONFIG_HOME" (merge-pathnames ".config/"))
         (setenv "XDG_CACHE_HOME" (merge-pathnames ".cache/"))
         (setenv "XDG_DATA_HOME" (merge-pathnames ".local/share/"))
         (unwind-protect
              ,@body
           (setenv "HOME" home)
           (setenv "XDG_CONFIG_HOME" config)
           (setenv "XDG_CACHE_HOME" cache)
           (setenv "XDG_DATA_HOME" data)
           (uiop:delete-directory-tree *default-pathname-defaults* :validate T))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program context

(define-cli-test infer-resource-files-without-root (run)
  (run)
  (is #'equal (uiop:xdg-data-home "leibowitz/") *data-directory*)
  (is #'equal (uiop:xdg-cache-home "leibowitz/") *cache-directory*)
  (is #'equal (user-homedir-pathname) *base-directory*)
  (of-type 'library *library*)
  (false *webserver*))

(define-cli-test infer-resource-files-with-root (run)
  (run "-r" ".")
  (is #'equal (merge-pathnames ".leibowitz/") *data-directory*)
  (is #'equal (merge-pathnames ".leibowitz/cache/") *cache-directory*)
  (is #'equal *default-pathname-defaults* *base-directory*)
  (of-type 'library *library*)
  (false *webserver*))

(define-cli-test help-and-return-error-on-no-subcommand (run)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: help

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: info

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: index

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: web

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: find

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: show

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: tag

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: tags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: untag

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: untags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: mv

(define-cli-test mv-simple-case-absolute-paths (run)
  (let* ((src (namestring (mktmp (user-homedir-pathname))))
         (dst (format NIl "~A_renamed" src)))
    (run "index" src)
    (run "mv" src dst)
    (true (get-datum *library* dst))
    (false (get-datum *library* src))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: cp

;;; FIXME: I'm really repeating myself here, the only thing I should
;;; be testing is that -f does indeed pass :overwrite.

(define-cli-test cp-simple-case-absolute-paths (run)
  (let* ((src (namestring (mktmp (user-homedir-pathname))))
         (dst (format NIL "~A_copy" src)))
    (run "index" src)
    (run "cp" src dst)
    (true (get-datum *library* src))
    (true (get-datum *library* dst))))

(define-cli-test cp-relative-path-for-destination (run)
  (let* ((src (namestring (mktmp (user-homedir-pathname)))))
    (run "index" src)
    (run "cp" src "hi")
    (true (get-datum *library* (truename "hi")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: rm

;; FIXME So much repetition from the core tests!

(define-cli-test remove-non-indexed-files-no-options (run)
  (let ((f1 (namestring (mktmp (user-homedir-pathname))))
        (f2 (namestring (mktmp (user-homedir-pathname)))))
    (true (probe-file f1))
    (true (probe-file f2))
    (fail (run "rm" f1 f2) 'datum-not-indexed)
    (true (probe-file f1))
    (true (probe-file f2))))

(define-cli-test remove-indexed-files-no-options (run)
  (let ((f (namestring (mktmp (user-homedir-pathname)))))
    (run "index" f)
    (true (probe-file f))
    (true (get-datum *library* f))
    (run "rm" f)
    (false (probe-file f))
    (false (get-datum *library* f))))

(define-cli-test remove-indexed-file-not-on-disk (run)
  (let ((f (namestring (mktmp (user-homedir-pathname)))))
    (run "index" f)
    (delete-file f)
    (run "rm" f)
    (false (get-datum *library* f))))

(define-cli-test attempt-to-remove-nonexistent-file (run)
  (let ((f1 (namestring (mktmp (user-homedir-pathname))))
        (f2 (namestring (mktmp (user-homedir-pathname)))))
    (run "index" f1)
    (run "index" f2)
    (fail (run "rm" f1 f2 "hopefully/no/such/file/or/directory")
        ;; FIXME: del-datum is idempotent and I prefer it that
        ;; way.... but it also makes the most sense for it to throw this
        ;; in order to remove the burden of error checking from the
        ;; caller
        'no-such-datum-in-disk-or-db)
    (true (probe-file f1))
    (true (probe-file f2))
    (true (get-datum *library* f1))
    (true (get-datum *library* f2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: ls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: show-tag

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: ls-tag

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: mv-tag

(define-cli-test mv-tag (run)
  (run "index" (namestring (mktmp (user-homedir-pathname)))) ;; initialize db
  (add-tag *library* "src")
  (run "mv-tag" "src" "dst")
  (false (get-tag *library* "src"))
  (true (get-tag *library* "dst")))

(define-cli-test mv-tag-overwrite (run)
  (run "index" (namestring (mktmp (user-homedir-pathname)))) ;; initialize db
  (add-tag *library* "src")
  (add-tag *library* "dst")
  (fail (run "mv-tag" "src" "dst"))
  (true (get-tag *library* "src"))
  (true (get-tag *library* "dst"))
  (run "mv-tag" "-f" "src" "dst")
  (false (get-tag *library* "src"))
  (true (get-tag *library* "dst")))

(define-cli-test mv-tag-merge (run)
  (run "index" (namestring (mktmp (user-homedir-pathname)))) ;; initialize db
  (add-tag *library* "src")
  (add-tag *library* "dst")
  (fail (run "mv-tag" "src" "dst"))
  (true (get-tag *library* "src"))
  (true (get-tag *library* "dst"))
  (run "mv-tag" "-m" "src" "dst")
  (false (get-tag *library* "src"))
  (true (get-tag *library* "dst")))

(define-cli-test mv-tag-both-args (run)
  (run "index" (namestring (mktmp (user-homedir-pathname)))) ;; initialize db
  (fail (run "mv-tag" "-o" "-m" "src" "dst")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: cp-tag

(define-cli-test cp-tag (run)
  (run "index" (namestring (mktmp (user-homedir-pathname)))) ;; initialize db
  (add-tag *library* "src")
  (run "cp-tag" "src" "dst")
  (true (get-tag *library* "src"))
  (true (get-tag *library* "dst")))

(define-cli-test cp-tag-overwrite (run)
  (run "index" (namestring (mktmp (user-homedir-pathname)))) ;; initialize db
  (add-tag *library* "src")
  (add-tag *library* "dst")
  (fail (run "cp-tag" "src" "dst"))
  (true (get-tag *library* "src"))
  (true (get-tag *library* "dst"))
  (run "cp-tag" "-f" "src" "dst")
  (true (get-tag *library* "src"))
  (true (get-tag *library* "dst")))

(define-cli-test cp-tag-merge (run)
  (run "index" (namestring (mktmp (user-homedir-pathname)))) ;; initialize db
  (add-tag *library* "src")
  (add-tag *library* "dst")
  (fail (run "cp-tag" "src" "dst"))
  (true (get-tag *library* "src"))
  (true (get-tag *library* "dst"))
  (run "cp-tag" "-m" "src" "dst")
  (true (get-tag *library* "src"))
  (true (get-tag *library* "dst")))

(define-cli-test cp-tag-both-args (run)
  (run "index" (namestring (mktmp (user-homedir-pathname)))) ;; initialize db
  (fail (run "cp-tag" "-o" "-m" "src" "dst")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subcommand: rm-tag

(define-cli-test remove-tag (run)
  (let ((path (namestring (mktmp (user-homedir-pathname)))))
    (run "index" path)
    (add-datum-tags *library* path '("some tag"))
    (is #'= 1 (length (get-datum-tags *library* path)))
    (true (get-tag *library* "some tag"))
    (run "rm-tag" "some tag")
    (false (get-tag *library* "some tag"))
    (false (get-datum-tags *library* path))))

