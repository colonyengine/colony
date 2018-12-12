(in-package :%fl)

(defclass display ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%window :reader window
            :initarg :window)
   (%hz :reader hz
        :initarg :hz)))

(defgeneric create-window (&key &allow-other-keys)
  (:method :before (&key major-version minor-version anti-alias-level)
    (sdl2:gl-set-attrs :context-major-version major-version
                       :context-minor-version minor-version
                       :context-profile-mask 1
                       :multisamplebuffers (if (zerop anti-alias-level) 0 1)
                       :multisamplesamples anti-alias-level))
  (:method (&key title width height)
    (let ((window (sdl2:create-window :title title :w width :h height :flags '(:opengl))))
      (sdl2:gl-create-context window)
      window)))

(defun maybe-set-vsync (value)
  (let ((value
          (ecase value
            (:on 1)
            (:off 0)
            (:adaptive -1))))
    (labels ((try (current-value)
               (handler-case (sdl2:gl-set-swap-interval current-value)
                 (sdl2::sdl-rc-error ()
                   (if (= current-value -1)
                       (try 1)
                       (v:warn :fl.core.display "Ignoring vsync option due to driver limitation."))))))
      (try value))))

(defgeneric make-display (core-state)
  (:method ((core-state core-state))
    (with-cfg (title window-width window-height) (context core-state)
      (let* ((context (context core-state))
             (window (create-window :title (cfg context :title)
                                    :width (cfg context :window-width)
                                    :height (cfg context :window-height)
                                    :major-version (cfg context :gl-version-major)
                                    :minor-version (cfg context :gl-version-minor)
                                    :anti-alias-level (cfg context :anti-alias-level)))
             (hz (nth-value 3 (sdl2:get-current-display-mode 0))))
        (setf (slot-value core-state '%display)
              (make-instance 'display
                             :window window
                             :core-state core-state
                             :hz hz))
        (v:info :fl.core.display "Display ~dx~d @ ~dHz created."
                window-width window-height hz)))))

(defmethod make-display :after ((core-state core-state))
  (with-cfg (gl-capabilities gl-blend-mode gl-depth-mode vsync) (context core-state)
    (apply #'gl:enable gl-capabilities)
    (apply #'gl:blend-func gl-blend-mode)
    (gl:depth-func gl-depth-mode)
    (maybe-set-vsync vsync)))

(defmethod clear-screen ((display display))
  (let* ((context (context (core-state display)))
         (elapsed-time (total-time context)))
    (multiple-value-call #'gl:clear-color
      (if (eq (cfg context :log-level) :debug)
          (values (* 0.25 (abs (sin elapsed-time))) 0 0 1)
          (values 0 0 0 1)))
    (gl:clear :color-buffer :depth-buffer)))

(defun render (core-state)
  (with-slots (%display %running-p) core-state
    (when %running-p
      (clear-screen %display)
      (execute-flow core-state
                    :default
                    'perform-one-frame
                    'entry/perform-one-frame
                    :come-from-state-name :ef)
      (sdl2:gl-swap-window (window %display)))))

(defmethod quit-display ((display display))
  (with-slots (%core-state %window) display
    (sdl2:destroy-window %window)
    (sdl2::sdl-quit)))
