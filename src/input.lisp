(in-package :fl.core)

(defstruct button-state
  (enabled nil)
  (enter nil)
  (leave nil))

(defun make-input-table ()
  (au:dict #'eq
           :mouse-button (au:dict #'eq)
           :mouse-scroll (au:dict #'eq)
           :mouse-move (au:dict #'eq)
           :key (au:dict #'eq)
           :gamepad-axis (au:dict #'equal)
           :gamepad-button (au:dict #'equal)))

(defun on-mouse-button-up (core-state button)
  (declare (ignore core-state button)))

(defun on-mouse-button-down (core-state button)
  (declare (ignore core-state button)))

(defun on-mouse-scroll-vertical (core-state amount)
  (declare (ignore core-state amount)))

(defun on-mouse-scroll-horizontal (core-state amount)
  (declare (ignore core-state amount)))

(defun on-mouse-move (core-state x y dx dy)
  (declare (ignore core-state x y dx dy)))

(defun on-key-up (core-state key)
  (declare (ignore core-state key)))

(defun on-key-down (core-state key)
  ;; TODO: Remove this later when possible.
  (when (eq key :escape)
    (stop-engine core-state)))

(defun on-gamepad-attach (core-state gamepad-id)
  (declare (ignore core-state gamepad-id)))

(defun on-gamepad-detach (core-state gamepad-id)
  (declare (ignore core-state gamepad-id)))

(defun on-gamepad-axis-move (core-state gamepad-id axis value)
  (declare (ignore core-state gamepad-id axis value)))

(defun on-gamepad-button-up (core-state gamepad-id button)
  (declare (ignore core-state gamepad-id button)))

(defun on-gamepad-button-down (core-state gamepad-id button)
  (declare (ignore core-state gamepad-id button)))
