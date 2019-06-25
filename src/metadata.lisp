(in-package #:%first-light)

(defvar *metadata-store* (u:dict))

(defun meta (key)
  (u:href *metadata-store* key))

(defun (setf meta) (value key)
  (setf (u:href *metadata-store* key) value))
