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

           ;; components
           #:basis
           #:camera
           #:tags
           #:transform
           #:add-child
           #:do-nodes ; sketchy!
           #:transform-node
           #:resolve-model))
