(in-package :%first-light)

(defclass context ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%project-data :accessor project-data)
   (%options :reader options
             :initarg :options)
   (%input-data :reader input-data
                :initarg :input-data)
   (%active-camera :accessor active-camera
                   :initform nil)
   (%shared-storage-table :reader shared-storage-table
                          :initform (fl.util:dict #'eq))
   (%state :accessor state
           :initform nil)))

(defun make-context (core-state)
  (setf (slot-value core-state '%context)
        (make-instance 'context
                       :core-state core-state
                       :input-data (input-data core-state)
                       :options (options core-state))))

(defun total-time (context)
  "Return the total time in seconds that the engine has been running."
  (slot-value (frame-manager (core-state context)) '%total-time))

(defun frame-time (context)
  "Return the amount of time in seconds of the last frame as a REAL."
  (slot-value (frame-manager (core-state context)) '%frame-time))

(defun delta (context)
  "Return the physics update delta. This is :delta from the cfg file."
  (slot-value (frame-manager (core-state context)) '%delta))

