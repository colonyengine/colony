(in-package #:virality.input)

(a:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defstruct mouse-motion-state x y dx dy)

;;; Events

(defun on-mouse-button-up (context button)
  (let ((data (v::input-data (v::core context))))
    (input-transition-out data (list :mouse button))
    (input-transition-out data '(:mouse :any))
    (input-transition-out data '(:button :any))))

(defun on-mouse-button-down (context button)
  (let ((data (v::input-data (v::core context))))
    (input-transition-in data (list :mouse button))
    (input-transition-in data '(:mouse :any))
    (input-transition-in data '(:button :any))))

(defun on-mouse-scroll (context x y)
  (let* ((data (v::input-data (v::core context)))
         (states (states data)))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (context new-x new-y new-dx new-dy)
  (let ((data (v::input-data (v::core context))))
    (with-slots (x y dx dy) (u:href (states data) '(:mouse :motion))
      (setf x new-x
            y new-y
            dx new-dx
            dy new-dy))))

;;; User protocol

(defun get-mouse-position (context)
  (let* ((data (v::input-data (v::core context)))
         (state (u:href (states data) '(:mouse :motion))))
    (with-slots (x y dx dy) state
      (values x y dx dy))))

(defun get-mouse-scroll (context axis)
  (let ((states (states (v::input-data (v::core context)))))
    (ecase axis
      (:horizontal (u:href states '(:mouse :scroll-horizontal)))
      (:vertical (u:href states '(:mouse :scroll-vertical))))))
