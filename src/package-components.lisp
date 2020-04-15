(in-package #:cl-user)

(defpackage #:virality.components
  (:use #:cl)
  ;; camera
  (:export
   #:active-p
   #:camera
   #:compute-camera-view
   #:find-active-camera
   #:following-camera
   #:projection
   #:tracking-camera
   #:transform
   #:view
   #:zoom-camera)
  ;; collider
  (:export
   #:collide-p
   #:on-layer
   #:referent
   #:sphere
   #:cuboid)
  ;; meshes
  (:export
   #:dynamic-mesh
   #:static-mesh)
  ;; render
  (:export
   #:material
   #:render)
  ;; sprite
  (:export
   #:frames
   #:name
   #:sprite)
  ;; transform
  (:export
   #:add-child
   #:local
   #:model
   #:remove-child
   #:transform))
