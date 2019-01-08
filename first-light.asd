(asdf:defsystem #:first-light
  :description "An experimental game engine."
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
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:closer-mop
               #:defpackage-plus
               #:cl-ppcre
               #:cl-graph
               #:cl-opengl
               #:sdl2
               #:verbose
               #:first-light.util
               #:first-light.metadata
               #:first-light.data-structures
               #:first-light.binary-formats
               #:first-light.math
               #:first-light.host
               #:first-light.image-types
               #:first-light.geometry
               #:first-light.input
               #:first-light.gpu
               #:first-light.gpu-lib)
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
     (:file "actions")
     (:file "api")))
   (:file "common")
   (:file "resource")
   (:file "context")
   (:file "options")
   (:file "logging")
   (:file "shared-storage")
   (:file "attributes")
   (:file "component-mop")
   (:file "component")
   (:file "core-state")
   (:file "frame")
   (:file "display")
   (:file "actor")
   (:file "flow")
   (:file "graph")
   (:file "scene")
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
   (:file "action-manager")
   (:module "components"
    :components
    ((:file "transform")
     (:file "action-list")
     (:file "camera")
     (:file "camera-following")
     (:file "camera-tracking")
     (:file "mesh")
     (:file "render")
     (:file "sprite")))
   (:module "actions"
    :components
    ((:file "fade")
     (:file "rotate")
     (:file "sprite-animate")))
   (:module "definitions"
    :components
    ((:file "graphs")
     (:file "flows")
     (:file "texture-profiles")
     (:file "textures")
     (:file "material-profiles")
     (:file "materials")))))
