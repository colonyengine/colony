(asdf:defsystem #:first-light.assets
  :description "Asset management for the First Light game engine."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:sdl2
               #:cl-opengl
               #:pngload
               #:parsley
               #:jsown)
  :serial t
  :components
  ((:file "package")
   (:file "gltf")
   (:file "mesh")))
