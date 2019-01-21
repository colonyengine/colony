(asdf:defsystem #:first-light.gpu
  :description "A system for defining shader programs and associated buffers."
  :author ("Michael Fiano <mail@michaelfiano.com>"
           "Peter Keller <psilord@cs.wisc.edu>"
           "Bart Botta <00003b at gmail.com>"
           "Elijah Malaby <djeis>")
  :maintainer ("Michael Fiano <mail@michaelfiano.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :homepage "https://github.com/hackertheory/first-light"
  :bug-tracker "https://github.com/hackertheory/first-light/issues"
  :source-control (:git "git@github.com:hackertheory/first-light.git")
  :encoding :utf-8
  :depends-on (#:static-vectors
               #:defpackage-plus
               #:glsl-packing
               #:varjo
               #:cl-opengl
               #:verbose
               #:first-light.util
               #:first-light.metadata
               #:first-light.math)
  :pathname "gpu"
  :serial t
  :components
  ((:file "package")
   (:file "shader")
   (:file "common")
   (:file "functions")
   (:file "stages")
   (:file "program")
   (:file "packing")
   (:file "attributes")
   (:file "uniforms")
   (:file "layout")
   (:file "blocks")
   (:file "buffers")))
