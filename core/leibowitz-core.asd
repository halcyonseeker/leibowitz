
(asdf:defsystem "leibowitz-core"
  :depends-on (#:sqlite
               #:local-time
               #:osicat
               #:cl-fad
               #:cl-who)
  :serial t
  :components ((:file "package")
               (:file "leibowitz")
               (:module "backends" :components ((:file "sqlite")))
               (:module "curators" :components (#+linux(:file "fs_inotify")))
               (:module "datatypes" :components ((:file "text")
                                                 (:file "link")))
               (:module "collections" :components ((:file "homedir")
                                                   (:file "link"))))
  :in-order-to ((asdf:test-op (asdf:test-op :leibowitz-core/tests))))

(asdf:defsystem "leibowitz-core/tests"
  :depends-on (#:parachute
               #:leibowitz-core)
  :serial t
  :components ((:file "integration_tests"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :leibowitz-core/tests)))
