(asdf:defsystem #:first-light.input
  :description "Input event system for first-light."
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
               #:golden-utils)
  :pathname "input"
  :serial t
  :components
  ((:file "package")
   (:file "keyboard")
   (:file "mouse")
   (:file "gamepad")
   (:file "window")
   (:file "states")
   (:file "input")))
