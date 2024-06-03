
(require 'asdf)

(loop for dir in (nconc (list #P"core/" #P"web/" #P"cli/")
                        (directory #P"lib/*/"))
      do (push dir asdf:*central-registry*))

(asdf:defsystem "leibowitz"
  :depends-on (#:leibowitz.util
               #:leibowitz.core
               #:leibowitz.web
               #:leibowitz.cli)
  :components ((:file "package"))
  :build-operation "program-op"
  :build-pathname "build/leibowitz"
  :entry-point "leibowitz.cli:main"
  :in-order-to ((asdf:test-op (asdf:test-op "leibowitz/tests"))))

(asdf:defsystem "leibowitz/tests"
  :depends-on (#:parachute
               #:leibowitz)
  :components ((:module "tests" :components ((:file "package")
                                             (:file "utils")
                                             (:file "core")
                                             ;;(:file "cli")
                                             )))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :leibowitz/tests)))
