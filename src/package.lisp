(in-package :cl-user)

(defpackage #:first-light
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export #:get-path
           #:scene-definition
           #:get-scene
           #:prepare-scenes
           #:context
           #:shaders
           #:make-core-state
           #:spawn-actor
           #:define-component
           #:component
           #:components
           #:display
           #:start-engine
           #:quit-engine
           #:key-down
           #:key-up
           #:map-nodes
           #:add-child

           ;; actors
           #:actor
           #:id
           #:make-actor

           ;; components
           #:make-component
           #:add-component
           #:add-multiple-components
           #:initialize-component
           #:physics-update-component
           #:make-camera-view
           #:update-component
           #:render-component
           #:destroy-component
           #:actor-components-by-type
           #:actor-component-by-type

           ;;; core components below:

           ;; basis
           #:basis

           ;; camera
           #:camera
           #:view
           #:projection
           #:tracking-camera
           #:look-at

           ;; mesh-renderer
           #:mesh-renderer

           ;; tags
           #:tags

           ;; transform
           #:transform
           #:model
           #:local))

(defpackage #:first-light-shaders
  (:use #:3bgl-glsl/cl))
