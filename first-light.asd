(asdf:defsystem #:first-light
  :description "An experimental game engine."
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
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:closer-mop
               #:defpackage-plus
               #:cl-ppcre
               #:queues.simple-cqueue
               #:cl-graph
               #:cl-opengl
               #:sdl2
               #:cl-tga
               #:verbose
               #:shadow
               #:umbra
               #:first-light.util
               #:first-light.metadata
               #:first-light.binary-formats
               #:first-light.math
               #:first-light.geometry)
  :pathname "core"
  :serial t
  :components
  ((:module "packages"
    :components
    ((:file "internal")
     (:file "shaders")
     (:file "materials")
     (:file "textures")
     (:file "annotations")
     (:file "components")
     (:file "api")))
   (:file "common")
   (:module "input"
    :components
    ((:file "keyboard")
     (:file "mouse")
     (:file "gamepad")
     (:file "window")
     (:file "states")
     (:file "input")))
   (:file "resource")
   (:file "context")
   (:file "extensions")
   (:file "settings")
   (:file "logging")
   (:file "attributes")
   (:file "component-mop")
   (:file "component")
   (:file "core-state")
   (:file "frame")
   (:file "display")
   (:file "actor")
   (:file "call-flow")
   (:file "type-dag")
   (:file "scene")
   (:file "image")
   (:file "texture")
   (:file "materials")
   (:file "shaders")
   (:file "engine")
   (:file "deploy")
   (:file "annotations")
   (:module "components"
    :components
    ((:file "transform")
     (:file "camera")
     (:file "camera-following")
     (:file "camera-tracking")
     (:file "mesh")
     (:file "mesh-renderer")))))
