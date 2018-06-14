(in-package :fl.core)

(au:define-constant +mouse-button-names+
    #(nil :mouse-left :mouse-middle :mouse-right :mouse-x1 :mouse-x2)
  :test #'equalp)

(defstruct mouse-motion-state
  (position (v2i:make 0 0))
  (delta (v2i:make 0 0)))

(defun set-cursor-hidden ()
  (sdl2:hide-cursor))

(defun set-cursor-visible ()
  (sdl2:show-cursor))

(defun on-mouse-button-up (core-state button)
  (declare (ignore core-state button)))

(defun on-mouse-button-down (core-state button)
  (declare (ignore core-state button)))

(defun on-mouse-scroll (core-state x y)
  (declare (ignore core-state))
  (unless (zerop x)
    (au:noop))
  (unless (zerop y)
    (au:noop)))

(defun on-mouse-move (core-state x y dx dy)
  (declare (ignore core-state x y dx dy)))
