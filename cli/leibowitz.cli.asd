
(asdf:defsystem "leibowitz.cli"
  :depends-on (#:clingon
               #:slynk
               #:alexandria
               #:leibowitz.core
               #:leibowitz.web)
  :components ((:file "package")
               (:file "conditions")
               (:file "main")))
