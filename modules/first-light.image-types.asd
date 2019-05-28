(asdf:defsystem #:first-light.image-types
  :description "Handles loading various image file formats."
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
  :depends-on (#:sdl2
               #:sdl2-image
               #:golden-utils)
  :pathname "image-types"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "loader-sdl2-image")))
