(asdf:defsystem #:first-light-example
  :description "Example scene for First Light."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:gamebox-math
               #:first-light)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "input")
   (:file "components/stub-0/package")
   (:file "components/stub-0/stub-0")
   (:file "components/gun/gun")
   (:file "components/gun-manager/gun-manager")
   (:file "components/hit-points/hit-points")
   (:file "components/spawn-destroy-test/spawn-destroy-test")))
