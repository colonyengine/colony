(asdf:defsystem #:first-light.example
  :description "Example scene for first-light."
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
   (:file "textures")
   (:file "shaders-damaged-helmet")
   (:file "shaders-graph-test")
   (:file "shaders-noise-test")
   (:file "shaders-texture-test")
   (:file "materials")
   (:file "scenes")
   (:file "components")
   (:file "sprite-test")))
