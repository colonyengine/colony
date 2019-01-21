(in-package :first-light.metadata)

(defvar *store* (u:dict #'eq))

(defun get (key)
  (u:href *store* key))

(defun set (key value)
  (setf (u:href *store* key) value))
