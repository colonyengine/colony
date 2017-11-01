(asdf:defsystem #:gear
  :description "An experimental game engine."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :homepage "https://github.com/hackertheory/gear"
  :bug-tracker "https://github.com/hackertheory/gear/issues"
  :source-control (:git "git@github.com:hackertheory/gear.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:gamebox-math)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "extensions")
   (:file "context")
   (:file "actor")
   (:file "components")
   (:file "core-state")
   (:file "call-flow")
   (:file "type-dag")
   (:file "scene")))
