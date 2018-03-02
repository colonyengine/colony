(in-package :fl.comp.mesh-renderer)

;; TODO: This file isn't used anymore since I now use a more general cache
;; api in core-state. Since that was the only thing being held in the
;; mesh-renderer's shared-storage, this can go. I'll remove it in a bit
;; if no other need for it arises.

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
