
(asdf:defsystem "leibowitz-cli"
  :depends-on (#:leibowitz-core
               #:leibowitz-web)
  :components ((:file "main")))
