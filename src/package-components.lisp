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
  ;; action
  (:export
   #:action
   #:actions
   #:action-list
   #:sprite-animate)
  ;; mesh
  (:export
   #:dynamic-mesh
   #:static-mesh)
  ;; render
  (:export
   #:draw-mesh
   #:material
   #:render)
  ;; sprite
  (:export
   #:sprite
   #:update-sprite-index)
  ;; transform
  (:export
   #:children
   #:interpolate-transforms
   #:local
   #:map-nodes
   #:model
   #:parent
   #:transform
   #:transform-node
   #:transform-add-child
   #:transform-remove-child
   #:translate
   #:rotate
   #:scale)
  ;; various colliders
  (:export
   #:center
   #:collider/sphere
   #:collide-p
   #:on-layer
   #:radius
   #:referent)
  ;; transform
  (:export
   #:inverse-transform-direction
   #:inverse-transform-point
   #:inverse-transform-vector
   #:local
   #:model
   #:rotate
   #:scale
   #:transform
   #:transform-add-child
   #:transform-backward
   #:transform-direction
   #:transform-down
   #:transform-forward
   #:transform-left
   #:transform-point
   #:transform-remove-child
   #:transform-right
   #:transform-up
   #:transform-vector
   #:translate))
