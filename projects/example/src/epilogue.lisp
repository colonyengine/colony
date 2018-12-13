(in-package #:first-light.example)

(defmethod epilogue (context)
  (v:trace :fl.core.engine "Running epilogue method."))
