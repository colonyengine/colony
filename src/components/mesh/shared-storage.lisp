(in-package :fl.comp.mesh)

(defclass mesh-shared-storage ()
  ((%cache :accessor cache
           :initform (make-hash-table :test #'equalp))))

(defclass cached-mesh ()
  ((%location :reader location
              :initarg :location)
   (%id :reader id
        :initarg :id)
   (%primitives :reader primitives
                :initarg :primitives)))

(defun make-cached-mesh (location id primitives)
  (make-instance 'cached-mesh :location location :id id :primitives primitives))

(defun cached-mesh (store location id)
  (gethash (cons location id) (cache store)))

(defun (setf cached-mesh) (value store location id)
  (setf (gethash (cons location id) (cache store)) value))
