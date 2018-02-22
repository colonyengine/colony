(asdf:defsystem #:first-light.shaders
  :description "GPU shaders and utilities for the First Light game engine."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:3bgl-shader)
  :serial t
  :components
  ((:file "package")
   (:file "shader-extension")
   (:file "3bgl-shader-stages")
   (:file "shader-stages")))
