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
  "Return the amount of time in seconds the last frame as a REAL."
  (box.frame:frame-time (display (core-state context))))

(defun delta (context)
  "Return the physics update delta. This is :delta from the cfg file."
  (cfg context :delta))
