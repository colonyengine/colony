(in-package #:virality.engine)

(defclass display ()
  ((%core :reader core
          :initarg :core)
   (%window :reader window
            :initarg :window)
   (%gl-context :reader gl-context
                :initarg :gl-context
                :initform nil)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)
   (%vsync-p :reader vsync-p
             :initarg :vsync-p)))

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
                       (log:warn :virality.engine
                                 "Ignoring vsync option due to driver ~
                                  limitation."))))))
      (try value))))

(defgeneric create-window (core)
  (:method :before (core)
    (let* ((opengl-version v:=opengl-version=)
           (anti-alias-level v:=anti-alias-level=))
      (u:mvlet ((major-version minor-version (parse-opengl-version
                                              opengl-version)))
        (sdl2:gl-set-attrs :context-major-version major-version
                           :context-minor-version minor-version
                           :context-profile-mask 1
                           :multisamplebuffers (max 0 (signum anti-alias-level))
                           :multisamplesamples anti-alias-level))))
  (:method (core)
    (let* ((window (sdl2:create-window :title v:=title=
                                       :w v:=window-width=
                                       :h v:=window-height=
                                       :flags '(:opengl)))
           (context (sdl2:gl-create-context window)))
      (values context window))))

(defmethod initialize-instance :after ((instance display)
                                       &key &allow-other-keys)
  (let ((core (core instance)))
    (setf (slot-value core '%display) instance)
    (gl:enable :depth-test :blend :multisample :cull-face)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (maybe-set-vsync v:=vsync=)))

(defun make-display (core)
  (u:mvlet ((gl-context window (create-window core))
            (refresh-rate (float (nth-value 3 (sdl2:get-current-display-mode 0))
                                 1d0)))
    (make-instance 'display
                   :core core
                   :window window
                   :gl-context gl-context
                   :refresh-rate refresh-rate
                   :vsync-p (eq v:=vsync= :on))))

(defmethod clear-screen ((display display))
  (let ((core (core display)))
    (multiple-value-call #'gl:clear-color
      (if (eq v:=log-level= :debug)
          (values (* 0.25 (abs (sin (total-time (context core))))) 0 0 1)
          (values 0 0 0 1)))
    (gl:clear :color-buffer :depth-buffer)))

(defun render-frame (core)
  (with-slots (%clock %display %running-p) core
    (when %running-p
      (clear-screen %display)
      (execute-flow core
                    :default
                    'perform-one-frame
                    'entry/perform-one-frame
                    :come-from-state-name :ef)
      (sdl2:gl-swap-window (window %display))
      (incf (clock-frame-count %clock)))))
