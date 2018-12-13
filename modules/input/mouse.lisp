(in-package :first-light.input)

(fl.util:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defstruct mouse-motion-state x y dx dy)

;;; Events

(defun on-mouse-button-up (input-data button)
  (input-transition-out input-data (list :mouse button))
  (input-transition-out input-data '(:mouse :any))
  (input-transition-out input-data '(:button :any)))

(defun on-mouse-button-down (input-data button)
  (input-transition-in input-data (list :mouse button))
  (input-transition-in input-data '(:mouse :any))
  (input-transition-in input-data '(:button :any)))

(defun on-mouse-scroll (input-data x y)
  (let ((states (fl.util:href (states input-data))))
    (unless (zerop x)
      (setf (fl.util:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (fl.util:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (input-data new-x new-y new-dx new-dy)
  (with-slots (x y dx dy) (fl.util:href (states input-data) '(:mouse :motion))
    (setf x new-x
          y new-y
          dx new-dx
          dy new-dy)))

;;; User protocol

(defun get-mouse-position (input-data)
  (let ((state (fl.util:href (states input-data) '(:mouse :motion))))
    (with-slots (x y dx dy) state
      (values x y dx dy))))

(defun get-mouse-scroll (input-data axis)
  (let ((states (states input-data)))
    (case axis
      (:horizontal (fl.util:href states '(:mouse :scroll-horizontal)))
      (:vertical (fl.util:href states '(:mouse :scroll-vertical))))))
