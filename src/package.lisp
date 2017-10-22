(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export
   ;; actor
   #:actor
   ;; components
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
   #:core-state
   #:make-core-state
   #:add-scene-tree-root
   #:add-initializing-actor))

(defpackage #:gear/example
  (:use #:cl
        #:gamebox-math
        #:gear))
