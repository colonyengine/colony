(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export #:component
           #:components
           #:add-component
           #:get-component
	   #:add-child
           #:transform
           #:make-transform
           #:tags
           #:game-object))

(defpackage #:gear/example
  (:use #:cl
        #:gamebox-math
        #:gear))
