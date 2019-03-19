(asdf:defsystem #:first-light.geometry
  :description "Constructs and loads geometrical assets."
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
  :depends-on (#:jsown
               #:cl-opengl
               #:defpackage-plus
               #:golden-utils
               #:first-light.binary-formats)
  :pathname "geometry"
  :serial t
  :components
  ((:file "package")
   (:file "gltf")))
