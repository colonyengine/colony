(in-package :first-light.input)

(u:define-constant +mouse-button-names+
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
  (let ((states (u:href (states input-data))))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (input-data new-x new-y new-dx new-dy)
  (with-slots (x y dx dy) (u:href (states input-data) '(:mouse :motion))
    (setf x new-x
          y new-y
          dx new-dx
          dy new-dy)))

;;; User protocol

(defun get-mouse-position (input-data)
  (let ((state (u:href (states input-data) '(:mouse :motion))))
    (with-slots (x y dx dy) state
      (values x y dx dy))))

(defun get-mouse-scroll (input-data axis)
  (let ((states (states input-data)))
    (case axis
      (:horizontal (u:href states '(:mouse :scroll-horizontal)))
      (:vertical (u:href states '(:mouse :scroll-vertical))))))
