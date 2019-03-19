(in-package :defpackage+-user-1)

(defpackage+ #:first-light.components
  (:nicknames #:fl.comp)
  (:local-nicknames (#:m #:game-math))
  (:use #:cl #:%first-light)
  ;; camera
  (:export #:camera
           #:tracking-camera
           #:following-camera
           #:transform
           #:view
           #:projection
           #:zoom-camera
           #:find-active-camera
           #:compute-camera-view)
  ;; action
  (:export #:action
           #:action-list
           #:sprite-animate)
  ;; mesh
  (:export #:mesh)
  ;; render
  (:export #:render
           #:material
           #:draw-mesh)
  ;; sprite
  (:export #:sprite
           #:update-sprite-index)
  ;; transform
  (:export #:transform
           #:transform-add-child
           #:transform-remove-child
           #:model
           #:local
           #:translate
           #:rotate
           #:scale))
