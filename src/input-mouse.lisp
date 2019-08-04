(in-package #:virality.engine)

(a:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defstruct mouse-motion-state x y dx dy)

;;; Events

(defun on-mouse-button-up (context button)
  (with-slots (%input-data) (core context)
    (input-transition-out %input-data (list :mouse button))
    (input-transition-out %input-data '(:mouse :any))
    (input-transition-out %input-data '(:button :any))))

(defun on-mouse-button-down (context button)
  (with-slots (%input-data) (core context)
    (input-transition-in %input-data (list :mouse button))
    (input-transition-in %input-data '(:mouse :any))
    (input-transition-in %input-data '(:button :any))))

(defun on-mouse-scroll (context x y)
  (let ((states (states (input-data (core context)))))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (context new-x new-y new-dx new-dy)
  (with-slots (x y dx dy) (u:href (states (input-data (core context)))
                                  '(:mouse :motion))
    (setf x new-x
          y new-y
          dx new-dx
          dy new-dy)))

;;; User protocol

(defun get-mouse-position (context)
  (let ((state (u:href (states (input-data (core context))) '(:mouse :motion))))
    (with-slots (x y dx dy) state
      (values x y dx dy))))

(defun get-mouse-scroll (context axis)
  (let ((states (states (input-data (core context)))))
    (ecase axis
      (:horizontal (u:href states '(:mouse :scroll-horizontal)))
      (:vertical (u:href states '(:mouse :scroll-vertical))))))
