(in-package #:cl-user)

(defpackage #:virality.engine
  (:use #:cl)
  (:import-from #:virality.geometry
                #:define-geometry
                #:define-geometry-layout)
  (:import-from #:virality.input
                #:get-gamepad-analog
                #:get-mouse-position
                #:get-window-mode
                #:get-window-size
                #:get-window-title
                #:input-enabled-p
                #:input-enter-p
                #:input-exit-p
                #:on-gamepad-analog-move
                #:on-gamepad-attach
                #:on-gamepad-button-down
                #:on-gamepad-button-up
                #:on-gamepad-detach
                #:on-window-close
                #:on-window-hide
                #:on-window-maximize
                #:on-window-minimize
                #:on-window-keyboard-focus-enter
                #:on-window-keyboard-focus-exit
                #:on-window-mouse-focus-enter
                #:on-window-mouse-focus-exit
                #:on-window-move
                #:on-window-resize
                #:on-window-restore
                #:on-window-show
                #:set-window-hidden
                #:set-window-mode
                #:set-window-title
                #:set-window-size
                #:set-window-visible)
  (:import-from #:virality.prefabs
                #:define-prefab
                #:define-prefab-descriptor
                #:prefab-descriptor
                #:make-prefab-instance
                #:ref)

  ;; common
  (:export
   #:destroy-after-time
   #:destroy
   #:display-id
   #:id
   #:with-shared-storage)

  ;; actions
  (:export
   #:action-step
   #:attrs
   #:manager
   #:on-action-finish
   #:on-action-insert
   #:on-action-update
   #:renderer
   #:repeat-p
   #:replace-action)

  ;; actors
  (:export
   #:actor
   #:make-actor
   #:spawn-actor)

  ;; components
  (:export
   #:actor-component-by-type
   #:actor-components-by-type
   #:attach-component
   #:attach-components
   #:attach-multiple-components
   #:make-component
   #:on-component-attach
   #:on-component-detach
   #:on-component-destroy
   #:on-component-initialize
   #:on-component-physics-update
   #:on-component-render
   #:on-component-update)

  ;; colliders
  (:export
   #:on-collision-enter
   #:on-collision-exit
   #:on-collision-continue)

  ;; context
  (:export
   #:context
   #:delta
   #:frame-count
   #:frame-time
   #:option
   #:total-time)

  ;; definitions
  (:export
   #:define-annotation
   #:define-call-flow
   #:define-component
   #:define-geometry
   #:define-geometry-layout
   #:define-graph
   #:define-material
   #:define-material-profile
   #:define-options
   #:define-prefab
   #:define-prefab-descriptor
   #:define-resources
   #:define-texture
   #:define-texture-profile)

  ;; input
  (:export
   #:get-gamepad-analog
   #:get-mouse-position
   #:get-window-mode
   #:get-window-size
   #:get-window-title
   #:input-enabled-p
   #:input-enter-p
   #:input-exit-p
   #:on-gamepad-analog-move
   #:on-gamepad-attach
   #:on-gamepad-button-down
   #:on-gamepad-button-up
   #:on-gamepad-detach
   #:on-window-close
   #:on-window-hide
   #:on-window-maximize
   #:on-window-minimize
   #:on-window-keyboard-focus-enter
   #:on-window-keyboard-focus-exit
   #:on-window-mouse-focus-enter
   #:on-window-mouse-focus-exit
   #:on-window-move
   #:on-window-resize
   #:on-window-restore
   #:on-window-show
   #:set-window-hidden
   #:set-window-mode
   #:set-window-title
   #:set-window-size
   #:set-window-visible)

  ;; materials
  (:export
   #:copy-material
   #:mat-uniform-ref
   #:with-material)

  ;; prefabs
  (:export
   #:make-prefab-instance
   #:prefab-descriptor
   #:ref))
