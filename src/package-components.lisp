(in-package #:cl-user)

(defpackage #:first-light.components
  (:nicknames #:fl.comp)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:~ #:origin.swizzle)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m4 #:origin.mat4)
                    (#:q #:origin.quat))
  (:use #:cl #:%first-light)
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
