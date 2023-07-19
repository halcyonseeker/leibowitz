
(loop for dir in (nconc (list #P"core/" #P"web/" #P"cli/")
                        (directory #P"lib/*/"))
      do (push dir asdf:*central-registry*))

(asdf:defsystem "leibowitz"
  :depends-on (#:leibowitz-core
               #:leibowitz-web
               #:leibowitz-cli)
  :components ((:file "package"))
  :build-operation "program-op"
  :build-pathname "build/leibowitz"
  :entry-point "leibowitz-cli:main")
