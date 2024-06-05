#!/usr/bin/env -S sbcl --script

(require 'asdf)
(require 'uiop)

(let ((ql (merge-pathnames #P"setup.lisp" (uiop:xdg-data-home "quicklisp/"))))
  (unless (probe-file ql)
    (warn "My quicklisp startup file ~S doesn't exist on this machine!" ql)
    (uiop:quit 1))
  (load ql))

(unless (probe-file "leibowitz.asd")
  (warn "Script must be run in the root of the repository!")
  (uiop:quit 1))

(defun dependency-names (system)
  (let ((found NIL))
    (labels ((%dependencies (sys)
               (loop for dep in (asdf:system-depends-on (asdf:find-system sys))
                     when (and (not (member dep found :test #'equal))
                               (stringp dep))
                       do (push dep found)
                          (%dependencies dep))))
      (%dependencies system))
    found))

(defun dependency-systems (names)
  (loop for name in names
        for release = (ql-dist:find-system name)
        if release collect release
          else do (format *error-output* "System not in quicklisp: ~S~%" name)))

(defun dependencies-checksum-url (systems)
  (loop for system in systems
        for release = (ql-dist:release system)
        for sha = (ql-dist:archive-content-sha1 release)
        for url = (ql-dist:archive-url release)
        do (format T "~20A ~A    ~A~%" (ql-dist:name release) sha url)
        collect (cons sha url)))

(load "leibowitz.asd")
(let ((*standard-output* *error-output*)) (ql:quickload :leibowitz))
(dependencies-checksum-url (dependency-systems (dependency-names "leibowitz")))
(dependencies-checksum-url (dependency-systems (dependency-names "leibowitz/tests")))
