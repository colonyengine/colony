(in-package #:cl-user)

(uiop:define-package #:virality.engine
  (:use #:cl)
  (:mix-reexport
   #:virality.colliders
   #:virality.geometry
   #:virality.materials
   #:virality.input
   #:virality.prefabs
   #:virality.textures)
  (:export
   #:actor
   #:context
   #:define-annotation
   #:define-assets
   #:define-call-flow
   #:define-graph
   #:define-options
   #:delta
   #:destroy-after-time
   #:destroy
   #:display-id
   #:frame-count
   #:frame-time
   #:id
   #:make-actor
   #:option
   #:spawn-actor
   #:start
   #:stop
   #:total-time
   #:with-shared-storage)
  ;; I'm working on these still - MF
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
