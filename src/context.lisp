(in-package :fl.core)

(defclass context ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%settings :reader settings
              :initform (au:dict #'eq))
   (%shaders :accessor shaders
             :initform nil)
   (%active-camera :accessor active-camera
                   :initform nil)
   (%shared-storage-table :reader shared-storage-table
                          :initform (au:dict #'eq))))

(defun frame-time (context)
  (box.frame:frame-time (display (core-state context))))
