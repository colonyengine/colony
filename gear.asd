(asdf:defsystem #:gear
  :description "An experimental game engine."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :homepage "https://github.com/hackertheory/gear"
  :bug-tracker "https://github.com/hackertheory/gear/issues"
  :source-control (:git "git@github.com:hackertheory/gear.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:sdl2
               #:sdl2kit
               #:glkit
               #:3bgl-shader
               #:simple-logger
               #:gamebox-math
               #:gamebox-frame-manager)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "logging")
   (:file "extensions")
   (:file "settings")
   (:file "context")
   (:file "actor")
   (:file "components")
   (:file "call-flow")
   (:file "core-state")
   (:file "type-dag")
   (:file "scene")
   (:file "display")
   (:file "input")
   (:file "shader")
   (:file "engine")
   (:file "shaders/shaders")
   (:file "components/basis/basis")
   (:file "components/camera/camera")
   (:file "components/camera/tracking-camera")
   (:file "components/tags/tags")
   (:file "components/transform/state")
   (:file "components/transform/transform")))
