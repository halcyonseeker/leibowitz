
(asdf:defsystem "thumbnailer"
  :depends-on (#:uiop
               #:zip
               #:lquery
               #:bordeaux-threads)
  :components ((:file "thumbnailer")))

