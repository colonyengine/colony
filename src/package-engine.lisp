(in-package #:cl-user)

(uiop:define-package #:virality.engine
  (:use #:cl)
  (:mix-reexport #:virality.actors
                 #:virality.colliders
                 #:virality.geometry
                 #:virality.materials
                 #:virality.input
                 #:virality.prefabs)

  ;; common
  (:export
   #:context
   #:delta
   #:destroy-after-time
   #:destroy
   #:display-id
   #:frame-count
   #:frame-time
   #:id
   #:option
   #:total-time
   #:with-shared-storage)

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

  ;; definitions
  (:export
   #:define-annotation
   #:define-call-flow
   #:define-component
   #:define-graph
   #:define-options
   #:define-resources
   #:define-texture
   #:define-texture-profile))
