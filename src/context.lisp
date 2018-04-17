(in-package :fl.core)

(defclass context ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%settings :reader settings
              :initform (make-hash-table))
   (%shaders :accessor shaders
             :initform nil)
   (%active-camera :accessor active-camera
                   :initform nil)
   (%shared-storage-table :reader shared-storage-table
                          :initform (make-hash-table))))


(defun lookup-material (material-name context)
  (multiple-value-bind (material presentp) (gethash material-name (materials (core-state context)))
    (if presentp
        material
        (gethash (au:ensure-symbol 'missing-material 'fl.materials)
                 (materials (core-state context))))))
