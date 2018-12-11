(in-package :%fl)

(fl.util:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defstruct mouse-motion-state x y dx dy)

;;; Events

(defun on-mouse-button-up (core-state button)
  (input-transition-out core-state (list :mouse button))
  (input-transition-out core-state '(:mouse :any))
  (input-transition-out core-state '(:button :any)))

(defun on-mouse-button-down (core-state button)
  (input-transition-in core-state (list :mouse button))
  (input-transition-in core-state '(:mouse :any))
  (input-transition-in core-state '(:button :any)))

(defun on-mouse-scroll (core-state x y)
  (let ((states (fl.util:href (states (input-data core-state)))))
    (unless (zerop x)
      (setf (fl.util:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (fl.util:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (core-state new-x new-y new-dx new-dy)
  (with-slots (x y dx dy) (fl.util:href (states (input-data core-state)) '(:mouse :motion))
    (setf x new-x
          y new-y
          dx new-dx
          dy new-dy)))

;;; User protocol

(defun get-mouse-position (context)
  (let ((state (fl.util:href (states (input-data (core-state context))) '(:mouse :motion))))
    (with-slots (x y dx dy) state
      (values x y dx dy))))

(defun get-mouse-scroll (context axis)
  (let ((states (states (input-data (core-state context)))))
    (case axis
      (:horizontal (fl.util:href states '(:mouse :scroll-horizontal)))
      (:vertical (fl.util:href states '(:mouse :scroll-vertical))))))
