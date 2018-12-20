(asdf:defsystem #:first-light.image-types
  :description "Handles loading various image file formats."
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
  :depends-on (#:cl-tga
               #:pngload
               #:verbose
               #:first-light.util)
  :pathname "image-types"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "type-targa")
   (:file "type-png")
   (:file "type-jpeg")))
