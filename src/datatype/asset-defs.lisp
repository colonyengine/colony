(in-package #:colony)

(defclass asset-spec ()
  ((%pool :reader pool
          :initarg :pool)
   (%name :reader name
          :initarg :name)
   (%path :reader path
          :initarg :path)))
