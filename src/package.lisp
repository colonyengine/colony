(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export

   ;; utilities
   #:get-path

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

   ;; transform component
   #:transform
   #:add-child

   ;; tags component
   #:tags

   ;; core-state
   #:make-core-state
   #:spawn-actor))
