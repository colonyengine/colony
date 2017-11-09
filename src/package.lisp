(in-package :cl-user)

(defpackage #:gear
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
           #:get-component
           #:display
           #:start-engine
           #:quit-engine
           #:key-down
           #:key-up
           #:map-nodes
           #:add-child

           ;; Actor API
           #:actor
           #:id ;; for component too.

           ;; Core component types
           #:basis
           #:tags

           ;; camera component
           #:camera
           #:view
           #:projection
           #:camera-look-at

           ;; transform component
           #:transform
           #:model
           #:local

           ;; Component handling
           #:make-component
           #:add-component
           #:add-multiple-components

           ;; component protocol
           #:initialize-component
           #:physics-update-component
           #:make-camera-view
           #:update-component
           #:render-component
           #:destroy-component))

(defpackage #:gear-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:vertex
           #:fragment))
