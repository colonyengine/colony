(asdf:defsystem #:first-light
  :description "An experimental game engine."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :homepage "https://github.com/hackertheory/first-light"
  :bug-tracker "https://github.com/hackertheory/first-light/issues"
  :source-control (:git "git@github.com:hackertheory/first-light.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:cl-graph
               #:split-sequence
               #:sdl2
               #:sdl2kit
               #:glkit
               #:3bgl-shader
               #:simple-logger
               #:gamebox-math
               #:gamebox-frame-manager
               #:defpackage-plus)
  :pathname "src"
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
   (:file "vertex-data")
   (:file "shader")
   (:file "engine")
   (:file "shaders/shaders")
   (:file "components/camera/package")
   (:file "components/camera/camera")
   (:file "components/camera/target-camera")
   (:file "components/tracking-camera/package")
   (:file "components/tracking-camera/tracking-camera")
   (:file "components/following-camera/package")
   (:file "components/following-camera/following-camera")
   (:file "components/mesh/package")
   (:file "components/mesh/mesh")
   (:file "components/mesh/mesh-format")
   (:file "components/mesh/mesh-shared-storage")
   (:file "components/mesh-renderer/package")
   (:file "components/mesh-renderer/mesh-renderer")
   (:file "components/tags/package")
   (:file "components/tags/tags")
   (:file "components/transform/state")
   (:file "components/transform/transform")))
