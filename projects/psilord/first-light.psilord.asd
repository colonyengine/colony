(asdf:defsystem #:first-light.psilord
  :description "Example scene for psilord's tests."
  :author ("Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:first-light
               #:defpackage-plus)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "resources")
   (:file "mat-test")
   (:module "shaders"
    :components
    ((:file "test-shader-0")))
   (:file "materials")))
