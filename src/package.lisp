(in-package :cl-user)

(defpackage #:gear
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export #:component
           #:components
           #:game-object))

(defpackage #:gear/example
  (:use #:cl
        #:gear))
