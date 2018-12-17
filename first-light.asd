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
               #:first-light.util
               #:first-light.metadata
               #:first-light.binary-formats
               #:first-light.math
               #:first-light.geometry
               #:first-light.input
               #:first-light.shader)
  :pathname "core"
  :serial t
  :components
  ((:module "packages"
    :components
    ((:file "internal")
     (:file "materials")
     (:file "textures")
     (:file "annotations")
     (:file "components")
     (:file "api")))
   (:file "common")
   (:file "resource")
   (:file "context")
   (:file "extensions")
   (:file "options")
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
   (:module "textures"
    :components
    ((:file "texture")
     (:file "common")
     (:file "1d")
     (:file "2d")
     (:file "3d")
     (:file "1d-array")
     (:file "2d-array")
     (:file "cube-map")
     (:file "cube-map-array")
     (:file "rectangle")
     (:file "buffer")))
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
