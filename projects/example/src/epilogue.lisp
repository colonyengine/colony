(in-package #:first-light.example)

(defun epilogue (context)
  (declare (ignore context))
  (v:trace :fl.core.engine "Running epilogue method."))
