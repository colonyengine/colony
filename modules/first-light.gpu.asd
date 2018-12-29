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
               #:glsl-packing
               #:varjo
               #:cl-opengl
               #:verbose
               #:first-light.util
               #:first-light.metadata)
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
   (:file "buffers")
   (:file "vari")
   (:module "shaders"
    :components
    ((:file "common")
     (:file "color-grading")
     (:file "color-space")
     (:file "graph")
     (:file "hashing-bbs")
     (:file "hashing-fast32")
     (:file "hashing-fast32-2")
     (:file "hashing-sgpp")
     (:file "math")
     (:file "noise-cellular")
     (:file "noise-hermite")
     (:file "noise-misc")
     (:file "noise-perlin")
     (:file "noise-polkadot")
     (:file "noise-simplex")
     (:file "noise-value")
     (:file "shaping-iq")
     (:file "shaping-levin")
     (:file "shaping-misc")
     (:file "shaping-penner")
     (:file "texture")))))
