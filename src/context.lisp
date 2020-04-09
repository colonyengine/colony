(in-package #:virality)

(defclass context ()
  ((%core :reader core
          :initarg :core)
   (%active-camera :accessor active-camera
                   :initform nil)
   (%shared-storage :reader shared-storage
                    :initform (u:dict))
   (%state :accessor state
           :initform nil)))

(defun make-context (core)
  (setf (slot-value core '%context) (make-instance 'context :core core)))

;; NOTE: This function must have &rest arguments, but they are ignored. This is
;; because this function is shared with the material protocol which must pass
;; more than the context.
(defun total-time (context &rest ignored)
  "Return the total time in seconds that the engine has been running."
  (declare (ignore ignored))
  (float (clock-total-time (clock (core context))) 1f0))

(defun frame-time (context)
  "Return the amount of time in seconds of the last frame as a REAL."
  (float (clock-frame-time (clock (core context))) 1f0))

(defun frame-count (context)
  "Return the number of frames since the engine has started."
  (floor (clock-frame-count (clock (core context)))))

(defun delta (context)
  "Return the physics update delta. This is :delta from the cfg file."
  (float (clock-delta-time (clock (core context))) 1f0))

(defun screen-resolution (context &rest ignored)
  (declare (ignore context ignored))
  (v2:vec (float v:=window-width= 1f0)
          (float v:=window-height= 1f0)))
