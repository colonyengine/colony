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
           #:make-component
           #:add-component
           #:add-multiple-components
           #:get-component
           #:display
           #:start-engine
           #:quit-engine
           #:key-down
           #:key-up

           ;; components
           #:basis
           #:camera
           #:tags
           #:transform
           #:add-child
           #:do-nodes))

(defpackage #:gear-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:vertex
           #:fragment))
