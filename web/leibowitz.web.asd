
(asdf:defsystem "leibowitz.web"
  :depends-on (#:leibowitz.core
               #:hunchentoot
               #:cl-who)
  :serial t
  :components ((:file "package")
               (:file "web")
               (:file "html")
               (:file "routes")))
