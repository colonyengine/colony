(in-package #:cl-user)

(defpackage #:virality.components.actions
  (:use #:cl)
  (:export
   #:actions))

(defpackage #:virality.components.camera
  (:use #:cl)
  (:export
   #:active-p
   #:camera
   #:compute-camera-view
   #:find-active-camera
   #:projection
   #:transform
   #:view
   #:zoom-camera))

(defpackage #:virality.components.camera.tracking
  (:use #:cl)
  (:export
   #:tracking-camera))

(defpackage #:virality.components.camera.following
  (:use #:cl)
  (:export
   #:following-camera))

(defpackage #:virality.components.collider
  (:use #:cl)
  (:export
   #:center
   #:collide-p
   #:on-layer
   #:radius
   #:referent
   #:sphere))

(defpackage #:virality.components.mesh.dynamic
  (:use #:cl)
  (:export #:dynamic-mesh))

(defpackage #:virality.components.mesh.static
  (:use #:cl)
  (:export #:static-mesh))

(defpackage #:virality.components.render
  (:use #:cl)
  (:export
   #:material
   #:render))

(defpackage #:virality.components.sprite
  (:use #:cl)
  (:export
   #:frames
   #:name
   #:sprite
   #:update-sprite-index))

(defpackage #:virality.components.transform
  (:use #:cl)
  (:export
   #:add-child
   #:inverse-transform-direction
   #:inverse-transform-point
   #:inverse-transform-vector
   #:local
   #:model
   #:remove-child
   #:rotate
   #:scale
   #:transform
   #:transform-backward
   #:transform-direction
   #:transform-down
   #:transform-forward
   #:transform-left
   #:transform-point
   #:transform-right
   #:transform-up
   #:transform-vector
   #:translate))
