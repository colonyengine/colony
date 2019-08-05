(in-package #:cl-user)

(uiop:define-package #:virality.engine
  (:use #:cl)
  (:mix-reexport #:virality.actors
                 #:virality.colliders
                 #:virality.geometry
                 #:virality.input
                 #:virality.prefabs)

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

  ;; components
  (:export
   #:component-by-type
   #:components-by-type
   #:attach-component
   #:attach-components
   #:make-component
   #:on-component-attach
   #:on-component-detach
   #:on-component-destroy
   #:on-component-initialize
   #:on-component-physics-update
   #:on-component-render
   #:on-component-update)

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
   #:define-graph
   #:define-material
   #:define-material-profile
   #:define-options
   #:define-resources
   #:define-texture
   #:define-texture-profile)

  ;; materials
  (:export
   #:copy-material
   #:mat-uniform-ref
   #:with-material))
