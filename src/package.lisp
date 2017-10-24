(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export
   ;; utilities
   #:path

   ;; actor
   #:actor

   ;; components
   #:define-component
   #:component
   #:components
   #:make-component
   #:add-component
   #:add-multiple-components
   #:get-component

   ;; scene dsl
   #:read-scene-file

   ;; transform component
   #:transform
   #:add-child

   ;; tags component
   #:tags

   ;; core-state
   #:core-state
   #:make-core-state
   #:add-scene-tree-root
   #:realize-actor))
