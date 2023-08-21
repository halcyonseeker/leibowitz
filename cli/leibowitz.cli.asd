
(asdf:defsystem "leibowitz.cli"
  :depends-on (#:clingon
               #:leibowitz.core
               #:leibowitz.web)
  :components ((:file "main")))
