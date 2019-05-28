(in-package #:first-light.metadata)

(defvar *store* (au:dict #'eq))

(defun get (key)
  (au:href *store* key))

(defun set (key value)
  (setf (au:href *store* key) value))
