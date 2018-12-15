(in-package :first-light.math)

(defun round-down (x)
  (cl:ceiling (cl:- x 1/2)))

(defun round-up (x)
  (cl:floor (cl:+ x 1/2)))
