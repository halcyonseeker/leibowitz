
(asdf:defsystem "leibowitz.web"
  :depends-on (#:leibowitz.core
               #:hunchentoot
               #:easy-routes
               #:cl-who)
  :serial t
  :components ((:file "package")
               (:file "web")
               (:file "html")
               (:file "routes")))
