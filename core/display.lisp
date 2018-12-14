(in-package :%first-light)

(defclass display ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%window :reader window
            :initarg :window)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)))

(defun parse-opengl-version (version)
  (values-list (mapcar #'parse-integer (fl.util:split-sequence #\. version))))

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
                       (v:warn :fl.core.display
                               "Ignoring vsync option due to driver limitation."))))))
      (try value))))

(defgeneric create-window (core-state)
  (:method :before ((core-state core-state))
    (let* ((context (context core-state))
           (opengl-version (option context :opengl-version))
           (anti-alias-level (option context :anti-alias-level)))
      (fl.util:mvlet ((major-version minor-version (parse-opengl-version opengl-version)))
        (sdl2:gl-set-attrs :context-major-version major-version
                           :context-minor-version minor-version
                           :context-profile-mask 1
                           :multisamplebuffers (signum anti-alias-level)
                           :multisamplesamples anti-alias-level))))
  (:method ((core-state core-state))
    (let* ((context (context core-state))
           (window (sdl2:create-window :title (option context :title)
                                       :w (option context :window-width)
                                       :h (option context :window-height)
                                       :flags '(:opengl))))
      (sdl2:gl-create-context window)
      window)))

(defmethod initialize-instance :after ((instance display) &key &allow-other-keys)
  (let ((core-state (core-state instance)))
    (setf (slot-value core-state '%display) instance)
    (gl:enable :depth-test :blend :multisample :cull-face)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (maybe-set-vsync (option (context core-state) :vsync))))

(defun make-display (core-state)
  (let ((window (create-window core-state)))
    (make-instance 'display
                   :core-state core-state
                   :window window
                   :refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))))

(defmethod clear-screen ((display display))
  (let ((core-state (core-state display)))
    (multiple-value-call #'gl:clear-color
      (if (eq (option core-state :log-level) :debug)
          (values (* 0.25 (abs (sin (total-time (context core-state))))) 0 0 1)
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
