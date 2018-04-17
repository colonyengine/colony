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
  (symbol-macrolet ((mat-table
		      (material-table (materials (core-state context)))))
    (au:if-found (material (au:href mat-table material-name))
                 material
                 (au:href mat-table
                          (au:ensure-symbol 'missing-material 'fl.materials)))))
