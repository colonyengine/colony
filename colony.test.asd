(asdf:defsystem #:colony.test
  :description "Tests for Colony Engine."
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"
  :homepage "https://github.com/colonyengine/colony"
  :depends-on  (#:colony
                #:trivial-custom-debugger
                #:parachute)
  :perform (asdf:test-op (o c) (uiop:symbol-call '#:parachute '#:test
                                                 '#:colony.test))
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:file "smoke")
   (:file "attribute-bag")))
