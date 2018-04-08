(asdf:defsystem #:first-light.engine
  :description "The core game engine for first-light."
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
               #:shadow
               #:gamebox-math.vari
               #:gamebox-frame-manager
               #:cl-tga)
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "utils")
   (:file "logging")
   (:file "extensions")
   (:file "context")
   (:file "settings")
   (:file "components")
   (:file "actor")
   (:file "call-flow")
   (:file "core-state")
   (:file "type-dag")
   (:file "scene")
   (:file "display")
   (:file "input")
   (:file "texture")
   (:file "materials")
   (:file "shaders")
   (:file "engine")))
