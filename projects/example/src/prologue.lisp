(in-package #:first-light.example)

(defun prologue (context)
  (declare (ignore context))
  (v:trace :fl.core.engine "Running prologue method."))
