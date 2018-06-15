(in-package :fl.core)

(au:define-constant +mouse-button-names+
    #(nil :mouse-left :mouse-middle :mouse-right :mouse-x1 :mouse-x2)
  :test #'equalp)

(defstruct mouse-motion-state x y dx dy)

(defun on-mouse-button-up (core-state button)
  (button-transition-out core-state (cons :mouse button)))

(defun on-mouse-button-down (core-state button)
  (button-transition-in core-state (cons :mouse button)))

(defun on-mouse-scroll (core-state x y)
  (let ((state (au:href (states (input-data core-state)))))
    (unless (zerop x)
      (setf (au:href state (cons :mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (au:href state (cons :mouse :scroll-vertical)) y))))

(defun on-mouse-move (core-state x y dx dy)
  (symbol-macrolet ((state (au:href (states (input-data core-state)) (cons :mouse :motion))))
    (setf (mouse-motion-state-x state) x
          (mouse-motion-state-y state) y
          (mouse-motion-state-dx state) dx
          (mouse-motion-state-dy state) dy)))
