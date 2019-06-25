(in-package #:%first-light)

(defclass display ()
  ((%core :reader core
          :initarg :core)
   (%window :reader window
            :initarg :window)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)))

(defun parse-opengl-version (version)
  (values-list (mapcar #'parse-integer
                       (split-sequence:split-sequence #\. version))))

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
                       (v:warn
                        :fl.core.display "Ignoring vsync option due to driver ~
                                          limitation."))))))
      (try value))))

(defgeneric create-window (core)
  (:method :before (core)
    (let* ((context (context core))
           (opengl-version (option context :opengl-version))
           (anti-alias-level (option context :anti-alias-level)))
      (u:mvlet ((major-version minor-version (parse-opengl-version
                                              opengl-version)))
        (sdl2:gl-set-attrs :context-major-version major-version
                           :context-minor-version minor-version
                           :context-profile-mask 1
                           :multisamplebuffers (signum anti-alias-level)
                           :multisamplesamples anti-alias-level))))
  (:method (core)
    (let* ((context (context core))
           (window (sdl2:create-window :title (option context :title)
                                       :w (option context :window-width)
                                       :h (option context :window-height)
                                       :flags '(:opengl))))
      (sdl2:gl-create-context window)
      window)))

(defmethod initialize-instance :after ((instance display)
                                       &key &allow-other-keys)
  (let ((core (core instance)))
    (setf (slot-value core '%display) instance)
    (gl:enable :texture-cube-map-seamless)
    (gl:enable :depth-test :blend :multisample :cull-face)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (maybe-set-vsync (option (context core) :vsync))))

(defun make-display (core)
  (let ((window (create-window core))
        (refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0))))
    (make-instance 'display
                   :core core
                   :window window
                   :refresh-rate refresh-rate)))

(defmethod clear-screen ((display display))
  (let ((core (core display)))
    (multiple-value-call #'gl:clear-color
      (if (eq (option core :log-level) :debug)
          (values (* 0.25 (abs (sin (total-time (context core))))) 0 0 1)
          (values 0 0 0 1)))
    (gl:clear :color-buffer :depth-buffer)))

(defun render (core)
  (with-slots (%frame-manager %display %running-p) core
    (with-slots (%frame-count) %frame-manager
      (when %running-p
        (clear-screen %display)
        (execute-flow core
                      :default
                      'perform-one-frame
                      'entry/perform-one-frame
                      :come-from-state-name :ef)
        (sdl2:gl-swap-window (window %display))
        (incf %frame-count)))))
