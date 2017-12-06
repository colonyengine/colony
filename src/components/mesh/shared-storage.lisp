(in-package :fl.comp.mesh)

(defclass mesh-shared-storage ()
  ((%cache :accessor cache
           :initform (make-hash-table :test #'equalp))))

(defclass cached-mesh ()
  ((%location :reader location
              :initarg :location)
   (%layout :reader layout
            :initarg :layout)
   (%vao :reader vao
         :initarg :vao)))

(defun make-cached-mesh (location layout vao)
  (make-instance 'cached-mesh :location location :layout layout :vao vao))

(defun cached-mesh (store location)
  (gethash location (cache store)))

(defun (setf cached-mesh) (value store location)
  (setf (gethash location (cache store)) value))
