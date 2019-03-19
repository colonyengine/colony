(asdf:defsystem #:first-light.gpu-lib
  :description "A standard library of re-usable GPU shaders."
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
  :depends-on (#:golden-utils
               #:first-light.gpu)
  :pathname "gpu-lib"
  :serial t
  :components
  ((:module "common"
    :components
    ((:file "package")
     (:file "common")
     (:file "swizzle")
     (:file "vari")
     (:file "math")
     (:file "structs")))
   (:module "color"
    :components
    ((:file "package")
     (:file "grading")
     (:file "space")))
   (:module "graph"
    :components
    ((:file "package")
     (:file "graph")))
   (:module "shaping"
    :components
    ((:file "package")
     (:file "iq")
     (:file "levin")
     (:file "misc")
     (:file "penner")))
   (:module "hashing"
    :components
    ((:file "package")
     (:file "bbs")
     (:file "fast32")
     (:file "fast32-2")
     (:file "sgpp")))
   (:module "noise"
    :components
    ((:file "package")
     (:file "cellular")
     (:file "hermite")
     (:file "misc")
     (:file "perlin")
     (:file "polkadot")
     (:file "simplex")
     (:file "value")))
   (:module "sdf"
    :components
    ((:file "package")
     (:file "2d")))
   (:module "texture"
    :components
    ((:file "package")
     (:file "texture")))
   (:module "sprite"
    :components
    ((:file "package")
     (:file "sprite")))))
