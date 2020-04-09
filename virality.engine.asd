(asdf:defsystem #:virality.engine
  :description "An experimental game engine."
  :author ("Michael Fiano <mail@michaelfiano.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <mail@michaelfiano.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :homepage "https://github.com/hackertheory/ViralityEngine"
  :bug-tracker "https://github.com/hackertheory/ViralityEngine/issues"
  :source-control (:git "https://github.com/hackertheory/ViralityEngine.git")
  :encoding :utf-8
  :depends-on (#:alexandria
               #:printv
               #:babel
               #:cl-cpus
               #:cl-graph
               #:cl-opengl
               #:cl-ppcre
               #:closer-mop
               #:doubly-linked-list
               #:fast-io
               #:global-vars
               #:golden-utils
               #:jsown
               #:lparallel
               #:origin
               #:pngload-fast
               #:queues.simple-cqueue
               #:sdl2
               #:shadow
               #:split-sequence
               #:static-vectors
               #:trivial-features
               #:uiop
               #:umbra
               #:verbose)
  :pathname "src"
  :serial t
  :components
  ((:file "package-region")
   (:file "package-colliders")
   (:file "package-components")
   (:file "package-extensions")
   (:file "package-geometry")
   (:file "package-materials")
   (:file "package-prefab")
   (:file "package-shader")
   (:file "package-textures")
   (:file "package-engine")
   (:file "package-nicknames")
   (:module "core-early"
    :components
    ((:file "general")
     (:file "metadata")
     (:file "config")
     (:file "hardware")
     (:file "thread-pool")
     (:file "live-coding")
     (:file "avl-tree")
     (:file "parser")
     (:file "uuid")))
   (:module "input"
    :components
    ((:file "data")
     (:file "keyboard")
     (:file "mouse")
     (:file "gamepad")
     (:file "window")
     (:file "button")
     (:file "input")))
   (:file "common")
   (:file "debugging")
   (:file "protocol")
   (:file "deployment")
   (:file "geometry-static")
   (:file "geometry-dynamic-attribute")
   (:file "geometry-dynamic-group")
   (:file "geometry-dynamic-buffer")
   (:file "geometry-dynamic")
   (:file "asset")
   (:file "rcache")
   (:file "clock")
   (:file "context")
   (:file "logging")
   (:file "graph")
   (:file "flow")
   (:file "attributes")
   (:file "actor")
   (:file "mop-component")
   (:file "component")
   (:file "shared-storage")
   (:file "kernel")
   (:file "annotations")
   (:file "shaders")
   (:file "region")
   (:file "bounding-volume-obb")
   (:file "colliders")
   (:file "texture-common")
   (:file "texture")
   (:file "texture-1d")
   (:file "texture-2d")
   (:file "texture-3d")
   (:file "texture-1d-array")
   (:file "texture-2d-array")
   (:file "texture-cube-map")
   (:file "texture-cube-map-array")
   (:file "texture-rectangle")
   (:file "texture-buffer")
   (:file "materials")
   (:module "components"
    :components
    ((:file "transform")
     (:file "camera")
     (:file "camera-following")
     (:file "camera-tracking")
     (:file "mesh-dynamic")
     (:file "mesh-static")
     (:file "render")
     (:file "sprite")
     (:file "collider-sphere")
     (:file "collider-cuboid")
     (:file "collider-collide-p")))
   (:module "prefab"
    :components
    ((:file "common")
     (:file "checks")
     (:file "parser")
     (:file "loader")
     (:file "reference")
     (:file "prefab")))
   (:module "core-late"
    :components
    ((:file "opengl")
     (:file "display")
     (:file "image")
     (:file "image-png")
     (:file "image-hdr")
     (:file "transform-state")
     (:file "transform-protocol")
     (:file "free-look-state")))
   (:file "core-state")
   (:file "engine")

   (:file "shader/texture")
   (:file "shader/visualization-collider")

   (:file "definition/annotations")
   (:file "definition/graphs")
   (:file "definition/flows")
   (:file "definition/texture-profiles")
   (:file "definition/textures")
   (:file "definition/material-profiles")
   (:file "definition/materials")))
