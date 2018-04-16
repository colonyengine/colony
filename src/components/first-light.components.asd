(asdf:defsystem #:first-light.components
  :description "Built-in components for the first-light game engine."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:module "transform"
    :components
    ((:file "transform")
     (:file "state")))
   (:module "camera"
    :components
    ((:file "camera")))
   (:module "following-camera"
    :components
    ((:file "following-camera")))
   (:module "tracking-camera"
    :components
    ((:file "tracking-camera")))
   (:module "mesh"
    :components
    ((:file "mesh")))
   (:module "mesh-renderer"
    :components
    ((:file "mesh-renderer")))))
