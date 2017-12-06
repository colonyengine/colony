(in-package :fl.comp.mesh-renderer)

(defclass mesh-renderer-shared-storage ()
  ((%texture-cache :accessor texture-cache
                   :initform (make-hash-table :test #'equalp))))

(defclass cached-texture ()
  ((%location :reader location
              :initarg :location)
   (%id :reader id
        :initarg :id)))

(defun make-cached-texture (location id)
  (make-instance 'cached-texture :location location :id id))

(defun cached-texture (store location)
  (gethash location (texture-cache store)))

(defun (setf cached-texture) (value store location)
  (setf (gethash location (texture-cache store)) value))
