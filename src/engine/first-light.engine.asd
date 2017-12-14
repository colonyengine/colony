(asdf:defsystem #:first-light.engine
  :description "The core game engine for First Light."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-ppcre
               #:split-sequence
               #:simple-logger
               #:cl-graph
               #:sdl2
               #:sdl2kit
               #:cl-opengl
               #:glkit
               #:gamebox-math
               #:gamebox-frame-manager
               #:pngload)
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "logging")
   (:file "extensions")
   (:file "context")
   (:file "settings")
   (:file "actor")
   (:file "components")
   (:file "call-flow")
   (:file "core-state")
   (:file "type-dag")
   (:file "scene")
   (:file "display")
   (:file "input")
   (:file "texture")
   (:file "materials")
   (:file "engine")))
