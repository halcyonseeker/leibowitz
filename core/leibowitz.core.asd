
(asdf:defsystem "leibowitz.core"
  :depends-on (#:sqlite
               #:local-time
               #:osicat
               #:cl-fad
               #:leibowitz.util
               #:thumbnailer)
  :serial t
  :components ((:file "package")
               (:file "leibowitz")
               (:file "conditions")
               (:module "backends" :components ((:file "sqlite")))
               (:module "curators" :components (#+linux(:file "fs_inotify")))
               (:module "datatypes" :components ((:file "text")
                                                 (:file "image")
                                                 (:file "video")
                                                 (:file "link")
                                                 (:module "application"
                                                          :components ((:file "pdf")))))
               (:module "collections" :components ((:file "homedir")
                                                   (:file "link")))))
