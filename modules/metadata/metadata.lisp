(in-package :first-light.metadata)

(defvar *store* (fl.util:dict #'eq))

(defun get (key)
  (fl.util:href *store* key))

(defun set (key value)
  (setf (fl.util:href *store* key) value))
