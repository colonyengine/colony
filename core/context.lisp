(in-package :%first-light)

(defclass context ()
  ((%core :reader core
          :initarg :core)
   (%project-data :accessor project-data)
   (%options :reader options
             :initarg :options)
   (%input-data :reader input-data
                :initarg :input-data)
   (%active-camera :accessor active-camera
                   :initform nil)
   (%shared-storage-table :reader shared-storage-table
                          :initform (au:dict #'eq))
   (%state :accessor state
           :initform nil)))

(defun make-context (core)
  (setf (slot-value core '%context)
        (make-instance 'context
                       :core core
                       :input-data (input-data core)
                       :options (options core))))

(defun total-time (context)
  "Return the total time in seconds that the engine has been running."
  (slot-value (frame-manager (core context)) '%total-time))

(defun frame-time (context)
  "Return the amount of time in seconds of the last frame as a REAL."
  (slot-value (frame-manager (core context)) '%frame-time))

(defun frame-count (context)
  "Return the number of frames since the engine has started."
  (slot-value (frame-manager (core context)) '%frame-count))

(defun delta (context)
  "Return the physics update delta. This is :delta from the cfg file."
  (slot-value (frame-manager (core context)) '%delta))
