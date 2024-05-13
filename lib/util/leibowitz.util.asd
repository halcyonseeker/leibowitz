
(asdf:defsystem "leibowitz.util"
  :depends-on (#:uiop
               #:hunchentoot
               #:cl-who)
  :components ((:file "util")))

