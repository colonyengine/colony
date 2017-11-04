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
      (setf (display core-state)
            (make-instance 'display
                           :core-state core-state
                           :title (cfg context :title)
                           :w (cfg context :width)
                           :h (cfg context :height)
                           :hz (calculate-refresh-rate)
                           :delta (cfg context :delta)
                           :period (cfg context :periodic-interval)
                           :debug-interval (cfg context :debug-interval))))))

(defmethod make-display :before ((core-state core-state))
  (let ((context (context core-state)))
    (dolist (attr `((:context-major-version ,(cfg context :gl-version-major))
                    (:context-minor-version ,(cfg context :gl-version-minor))
                    (:multisamplebuffers
                     ,(if (zerop (cfg context :anti-alias-level)) 0 1))
                    (:multisamplesamples ,(cfg context :anti-alias-level))))
      (apply #'sdl2:gl-set-attr attr))))

(defmethod make-display :after ((core-state core-state))
  (let ((context (context core-state)))
    (setf (kit.sdl2:idle-render (display core-state)) t)
    (apply #'gl:enable (cfg context :gl-capabilities))
    (apply #'gl:blend-func (cfg context :gl-blend-mode))
    (gl:depth-func (cfg context :gl-depth-mode))
    (sdl2:gl-set-swap-interval (if (cfg context :vsync) 1 0))))

(defmethod kit.sdl2:render ((display display))
  (gl:clear :color-buffer :depth-buffer)
  (execute-flow (core-state display)
                :default
                'perform-one-frame
                'entry/perform-one-frame
                :come-from-state-name :ef)
  ;; TODO: render pass
  nil)

(defmethod quit ((display display))
  (kit.sdl2:close-window display)
  (kit.sdl2:quit))
