(asdf:defsystem #:first-light.example
  :description "Examples for first-light."
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
   (:module "shaders"
    :components
    ((:file "damaged-helmet")
     (:file "graph")
     (:file "noise")
     (:file "texture")))
   (:file "example-collision")
   (:file "example-damaged-helmet")
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
