(asdf:defsystem #:first-light.shaderlib
  :description "A system for defining shader programs and associated buffers."
  :author ("Michael Fiano <mail@michaelfiano.com>"
           "Peter Keller <psilord@cs.wisc.edu>"
           "Bart Botta <00003b at gmail.com>")
  :maintainer ("Michael Fiano <mail@michaelfiano.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :homepage "https://github.com/hackertheory/first-light"
  :bug-tracker "https://github.com/hackertheory/first-light/issues"
  :source-control (:git "git@github.com:hackertheory/first-light.git")
  :encoding :utf-8
  :depends-on (#:static-vectors
               #:glsl-packing
               #:varjo
               #:cl-opengl
               #:first-light.util
               #:first-light.metadata)
  :pathname "shaderlib"
  :serial t
  :components
  ((:file "package")
   (:file "shaderlib")
   (:file "common")
   (:file "functions")
   (:file "stages")
   (:file "program")
   (:file "packing")
   (:file "attributes")
   (:file "uniforms")
   (:file "layout")
   (:file "blocks")
   (:file "buffers")
   (:file "vari")))
