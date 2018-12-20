(asdf:defsystem #:first-light.data-structures
  :description "Various data structures used by first-light."
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
  :depends-on (#:queues.simple-cqueue
               #:defpackage-plus
               #:first-light.util)
  :pathname "data-structures"
  :serial t
  :components
  ((:file "package")
   (:file "doubly-linked-list")))
