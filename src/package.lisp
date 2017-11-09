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

           ;; actor API
           #:actor
           #:id ; for component too.

           ;; core component types
           #:basis
           #:tags

           ;; camera component
           #:camera
           #:view
           #:projection
           #:tracking-camera
           #:look-at

           ;; transform component
           #:transform
           #:model
           #:local

           ;; component handling
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
  (:use #:3bgl-glsl/cl))
