(in-package #:colony)

(defclass geometry-layout ()
  ((%name :reader name
          :initarg :name)
   (%groups :accessor groups)
   (%group-order :accessor group-order)))
