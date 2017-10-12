(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export #:component
           #:components
           #:transform
           #:game-object))

(defpackage #:gear/example
  (:use #:cl
        #:gear))
