;; fs_inotify.lisp — Manage library data as files on Linux.

(in-package :leibowitz-core)

(defclass fs-curator (curator)
  ()
  (:documentation "Implementation of `curator' that uses the inotify(7) API on Linux to
make sure the library data is kept up-to-date with respect to the
files on the file system."))
