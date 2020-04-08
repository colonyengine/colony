(in-package #:cl-user)

(uiop:define-package #:virality.engine
  (:use #:cl)
  (:mix-reexport
   #:virality.colliders
   #:virality.geometry
   #:virality.materials
   #:virality.prefabs
   #:virality.textures)
  (:export
   #:actor
   #:context
   #:define-annotation
   #:define-assets
   #:define-call-flow
   #:define-config
   #:define-graph
   #:delta
   #:destroy
   #:display-id
   #:frame-count
   #:frame-time
   #:id
   #:make-actor
   #:screen-resolution
   #:spawn-actor
   #:start
   #:stop
   #:total-time
   #:with-shared-storage)

  ;; hardware
  (:export
   #:get-hardware-info)

  ;; input system
  (:export
   #:disable-relative-motion
   #:enable-relative-motion
   #:get-gamepad-analog
   #:get-mouse-position
   #:get-mouse-scroll
   #:get-window-mode
   #:get-window-size
   #:get-window-title
   #:mouse-motion-relative-p
   #:on-button-enabled
   #:on-button-enter
   #:on-button-exit
   #:on-gamepad-analog-move
   #:on-gamepad-attach
   #:on-gamepad-detach
   #:on-gamepad-enabled
   #:set-window-hidden
   #:set-window-mode
   #:set-window-title
   #:set-window-size
   #:set-window-visible)

  ;; transform protocol
  (:export
   #:get-model-matrix
   #:get-rotation
   #:get-scale
   #:get-translation
   #:rotate
   #:rotate/velocity
   #:scale
   #:scale/velocity
   #:transform-backward
   #:transform-direction
   #:transform-down
   #:transform-forward
   #:transform-left
   #:transform-point
   #:transform-right
   #:transform-up
   #:transform-vector
   #:translate
   #:translate/velocity)

  (:export
   #:attach-component
   #:attach-components
   #:component-by-type
   #:define-component
   #:make-component
   #:on-component-attach
   #:on-component-detach
   #:on-component-destroy
   #:on-component-initialize
   #:on-component-physics-update
   #:on-component-render
   #:on-component-update))
