(asdf:defsystem #:first-light
  :description "An experimental game engine."
  :author ("Michael Fiano <mail@michaelfiano.com>"
           "Peter Keller <psilord@cs.wisc.edu>"
           "|3b| in #lispgames and ##hacker-theory on freenode IRC")
  :maintainer ("Michael Fiano <mail@michaelfiano.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :homepage "https://github.com/hackertheory/first-light"
  :bug-tracker "https://github.com/hackertheory/first-light/issues"
  :source-control (:git "git@github.com:hackertheory/first-light.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:serapeum
               #:closer-mop
               #:defpackage-plus
               #:cl-ppcre
               #:queues.simple-cqueue
               #:cl-graph
               #:jsown
               #:cl-opengl
               #:sdl2
               #:cl-tga
               #:simple-logger
               #:parsley
               #:shadow
               #:umbra
               #:gamebox-math.vari
               #:gamebox-frame-manager)
  :pathname "src"
  :serial t
  :components
  ((:file "package-internal")
   (:file "package-api")
   (:file "util")
   (:file "common")
   (:file "logging")
   (:file "resource")
   (:file "context")
   (:file "extensions")
   (:file "settings")
   (:file "attributes")
   (:file "components-mop")
   (:file "components")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-gamepad")
   (:file "input-window")
   (:file "input-states")
   (:file "input")
   (:file "core-state")
   (:file "display")
   (:file "actor")
   (:file "call-flow")
   (:file "type-dag")
   (:file "transform-state")
   (:file "scene")
   (:file "viewport")
   (:file "image")
   (:file "texture")
   (:file "materials")
   (:file "shaders")
   (:file "mesh-parser")
   (:file "engine")
   (:file "deploy")
   (:file "annotations")
   (:file "components/transform")
   (:file "components/camera")
   (:file "components/camera-following")
   (:file "components/camera-tracking")
   (:file "components/mesh")
   (:file "components/mesh-renderer")))
