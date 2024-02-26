(asdf:defsystem #:virality.test
  :description "Tests for Virality Engine."
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"
  :homepage "https://github.com/bufferswap/ViralityEngine"
  :depends-on  (#:virality
                #:trivial-custom-debugger
                #:parachute)
  :perform (asdf:test-op (o c) (uiop:symbol-call '#:parachute '#:test
                                                 '#:virality.test))
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:file "smoke")))
