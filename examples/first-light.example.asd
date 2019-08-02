(asdf:defsystem #:first-light.example
  :description "Virality Engine Examples"
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:first-light)
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "shaders/damaged-helmet")
   (:file "shaders/dynamic-geometry")
   (:file "shaders/graph")
   (:file "shaders/noise")
   (:file "shaders/texture")
   (:file "example-collision")
   (:file "example-damaged-helmet")
   (:file "example-dynamic-geometry")
   (:file "example-geometric-volumes")
   (:file "example-graph")
   (:file "example-isometric-view")
   (:file "example-noise")
   (:file "example-sprite")
   (:file "example-texture")
   (:module "protect-the-planets"
    :components
    ((:file "common")
     (:file "example-protect-the-planets")))))
