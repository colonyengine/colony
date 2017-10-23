(asdf:defsystem #:gear-example
  :description "Example scene for gear."
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
               #:gamebox-math
               #:gear)
  :pathname "example"
  :serial t
  :components
  ((:file "package")
   (:file "test-scene")
   (:module "components/gun"
    :components
    ((:file "gun")))
   (:module "components/gun-manager"
    :components
    ((:file "gun-manager")))
   (:module "components/hit-points"
    :components
    ((:file "hit-points")))))
