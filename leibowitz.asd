
(loop for dir in '(#P"core/")
      do (push dir asdf:*central-registry*))

(asdf:defsystem "leibowitz"
  :depends-on (#:leibowitz-core)
  :components ((:file "main"))
  :build-operation "program-op"
  :build-pathname "build/leibowitz"
  :entry-point "leibowitz:main")
