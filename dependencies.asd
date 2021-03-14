;;; Filename: dependencies.asd


(defsystem "dependencies"
  :description "dependencies: A Common Lisp system for detecting dependencies
                between a project's files."
  :version "1.1"
  :author "David Brown <davypough@gmail.com>"
  :licence "Public Domain"
  :depends-on ("alexandria")
  :components ((:file "dependencies")))
