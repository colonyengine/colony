(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export #:get-path
           #:actor
           #:scene-definition
           #:get-scene
           #:prepare-scenes
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
           #:do-nodes
           #:add-child

           ;; Core component types
           #:basis
           #:camera
           #:tags
           #:transform

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
