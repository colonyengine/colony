(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export #:component
           #:components
           #:make-component
           #:add-component
           #:add-multiple-components
           #:get-component
           #:add-child
           #:transform
           #:tags
           #:actor))

(defpackage #:gear/example
  (:use #:cl
        #:gamebox-math
        #:gear))
