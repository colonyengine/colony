(asdf:defsystem #:first-light.shaders
  :description "GPU shaders and utilities for the first-light game engine."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:shadow)
  :serial t
  :components
  ((:file "package")))
