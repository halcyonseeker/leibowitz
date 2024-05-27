
(asdf:defsystem "leibowitz.web"
  :depends-on (#:leibowitz.core
               #:leibowitz.util
               #:alexandria
               #:hunchentoot
               #:easy-routes
               #:cl-who)
  :serial t
  :components ((:file "package")
               (:file "web")
               (:file "html")
               (:file "routes")))
