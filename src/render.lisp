(in-package :gear)

(defclass display (kit.sdl2:gl-window box.fm:frame-manager)
  ((core-state :reader core-state
               :initarg :core-state)
   (hz :reader hz
       :initarg :hz)))

(defun calculate-refresh-rate ()
  (let ((hz (nth-value 3 (sdl2:get-current-display-mode 0))))
    (if (zerop hz) 60 hz)))

(defgeneric make-display (core-state)
  (:method ((core-state core-state))
    (let ((context (context core-state)))
      (setf (slot-value context 'display)
            (make-instance 'display
                           :core-state core-state
                           :title (title context)
                           :w (width context)
                           :h (height context)
                           :hz (calculate-refresh-rate)
                           :delta (delta context)
                           :period (periodic-interval context)
                           :debug-interval (debug-interval context))))))

(defmethod make-display :before ((core-state core-state))
  (let ((context (context core-state)))
    (dolist (attr `((:context-major-version ,(gl-version-major context))
                    (:context-minor-version ,(gl-version-minor context))
                    (:multisamplebuffers ,(if (zerop (anti-alias-level context)) 0 1))
                    (:multisamplesamples ,(anti-alias-level context))))
      (apply #'sdl2:gl-set-attr attr))))

(defmethod make-display :after ((core-state core-state))
  (let ((context (context core-state)))
    (setf (kit.sdl2:idle-render (slot-value context 'display)) t)
    (apply #'gl:enable (gl-capabilities context))
    (apply #'gl:blend-func (gl-blend-mode context))
    (gl:depth-func (gl-depth-mode context))
    (sdl2:gl-set-swap-interval (if (vsyncp context) 1 0))))

(defmethod kit.sdl2:render ((display display))
  (let ((*core-state* (core-state display)))
    (gl:clear :color-buffer :depth-buffer)
    (box.fm:tick display (hz display) (lambda () #++(update *core-state*))
                 :periodic-func (lambda () #++(periodic-update *core-state*)))
    #++(render *core-state* (box.fm:alpha display))))

(defmethod kit.sdl2:close-window :around ((display display))
  (let* ((*core-state* (core-state display)))
    (call-next-method)
    (kit.sdl2:quit)))
