(in-package :fl.core)

(defstruct button-state
  (on nil)
  (transition-on nil)
  (transition-off nil))

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

(defun on-mouse-move (core-state id x y xrel yrel)
  (declare (ignore core-state id x y xrel yrel)))

(defun on-key-up (core-state key)
  (declare (ignore core-state key)))

(defun on-key-down (core-state key)
  (declare (ignore core-state key)))

(defun on-gamepad-attach (core-state id)
  (declare (ignore core-state id)))

(defun on-gamepad-detach (core-state id)
  (declare (ignore core-state id)))

(defun on-gamepad-axis-move (core-state gamepad-id axis value)
  (declare (ignore core-state gamepad-id axis value)))

(defun on-gamepad-button-up (core-state id button)
  (declare (ignore core-state id button)))

(defun on-gamepad-button-down (core-state id button)
  (declare (ignore core-state id button)))
