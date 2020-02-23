(in-package #:virality.input)

(a:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defstruct mouse-motion-state
  relative
  (warp-x 0)
  (warp-y 0)
  (x 0)
  (y 0)
  (dx 0)
  (dy 0))

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

;;; TODO: Update option table when display width/height changes.
(defun on-mouse-move (context x y dx dy)
  (let* ((data (v::input-data (v::core context)))
         (motion-state (u:href (states data) '(:mouse :motion)))
         (window-height (v::option context :window-height))
         (relative (sdl2:relative-mouse-mode-p)))
    ;; TODO: fix this?
    (unless relative
      (setf (mouse-motion-state-x motion-state) x
            (mouse-motion-state-y motion-state) (- window-height y)))
    (setf (mouse-motion-state-dx motion-state) dx
          (mouse-motion-state-dy motion-state) (- dy))))

(defun reset-mouse-state (data)
  (let* ((states (states data))
         (motion-state (u:href states '(:mouse :motion))))
    (setf (u:href states '(:mouse :scroll-horizontal)) 0
          (u:href states '(:mouse :scroll-vertical)) 0
          (mouse-motion-state-dx motion-state) 0
          (mouse-motion-state-dy motion-state) 0)))

;;; User protocol

(defun get-mouse-position (context)
  (let* ((data (v::input-data (v::core context)))
         (motion-state (u:href (states data) '(:mouse :motion)))
         (x (mouse-motion-state-x motion-state))
         (y (mouse-motion-state-y motion-state))
         (dx (mouse-motion-state-dx motion-state))
         (dy (mouse-motion-state-dy motion-state)))
    (values x y dx dy)))

(defun get-mouse-scroll (context axis)
  (let ((states (states (v::input-data (v::core context)))))
    (ecase axis
      (:horizontal (u:href states '(:mouse :scroll-horizontal)))
      (:vertical (u:href states '(:mouse :scroll-vertical))))))

(defun enable-relative-motion (context)
  (let* ((input-data (v::input-data (v::core context)))
         (motion-state (u:href (states input-data) '(:mouse :motion)))
         (window-height (v::option context :window-height))
         (x (mouse-motion-state-x motion-state))
         (y (- window-height (mouse-motion-state-y motion-state))))
    (sdl2:set-relative-mouse-mode 1)
    (setf (mouse-motion-state-relative motion-state) t
          (mouse-motion-state-warp-x motion-state) x
          (mouse-motion-state-warp-y motion-state) y)))

(defun disable-relative-motion (context &key (warp t))
  (let* ((input-data (v::input-data (v::core context)))
         (motion-state (u:href (states input-data) '(:mouse :motion))))
    (sdl2:set-relative-mouse-mode 0)
    (setf (mouse-motion-state-relative motion-state) nil)
    (when warp
      (let ((warp-x (mouse-motion-state-warp-x motion-state))
            (warp-y (mouse-motion-state-warp-y motion-state)))
        (sdl2:warp-mouse-in-window nil warp-x warp-y)))))

(defun mouse-motion-relative-p (context)
  (let* ((input-data (v::input-data (v::core context)))
         (motion-state (u:href (states input-data) '(:mouse :motion))))
    (mouse-motion-state-relative motion-state)))
