(asdf:defsystem #:first-light.binary-formats
  :description "Tools for loading and parsing binary file formats."
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
  :depends-on (#:fast-io
               #:bitio
               #:chipz
               #:babel)
  :pathname "binary-formats"
  :serial t
  :components
  ((:file "package")
   (:file "buffer")
   (:file "common")
   (:file "readers")))
