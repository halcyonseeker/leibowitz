
(asdf:defsystem "leibowitz.cli"
  :depends-on (#:clingon
               #:slynk
               #:leibowitz.core
               #:leibowitz.web)
  :components ((:file "package")
               (:file "conditions")
               (:file "main")))
