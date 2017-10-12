(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export #:component
           #:components
           #:add-component
           #:get-component
           #:transform
           #:make-transform
           #:tags
           #:game-object))

(defpackage #:gear/example
  (:use #:cl
        #:gamebox-math
        #:gear))
