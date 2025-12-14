(in-package #:colony)

;;;; implementation of DISPLAY structure

(defun parse-opengl-version ()
  (mapcar #'parse-integer
          (split-sequence:split-sequence #\. =opengl-version=)))

(defun set-opengl-attributes ()
  (destructuring-bind (major minor) (parse-opengl-version)
    (sdl2:gl-set-attrs
     :context-major-version major
     :context-minor-version minor
     :context-profile-mask sdl2-ffi:+SDL-GL-CONTEXT-PROFILE-CORE+
     :doublebuffer 1
     ;; TODO: These next two are sometimes problematic between GPU vendors.
     ;; NVidia likes 16 for :multisamplesamples but AMD likes 8. Since we don't
     ;; currently have a way to figure this sort of thing out automatically, we
     ;; choose the conservative value for now.
     :multisamplebuffers 1
     :multisamplesamples 8)))

(defun make-opengl-context (display)
  (let ((context (sdl2:gl-create-context (window display))))
    (setf (context display) context)
    (apply #'gl:enable +enabled-capabilities+)
    (apply #'gl:disable +disabled-capabilities+)
    (apply #'gl:blend-func +blend-mode+)
    (gl:pixel-store :unpack-alignment 1)
    (gl:depth-func +depth-mode+)))

(defun make-window ()
  ;; opengl attributes must be set BEFORE SDL window creation.
  (set-opengl-attributes)
  (sdl2:create-window :title =window-title=
                      :w (truncate =window-width=)
                      :h (truncate =window-height=)
                      :flags '(:opengl)))

(defun make-display (core)
  ;; (sdl2:init :everything)
  ;;
  ;; NOTE: We can't call (sdl2:init :everything) because it tries to manage the
  ;; main thread itself and when there is an ABORT restart, will get confused
  ;; and lock up. Since we do the thread management ourselves, we the the lower
  ;; level raw (an unexported, hence ::) equvalent of it in the SDL2 CFFI.
  ;;
  ;; TODO: Prolly should export this in SDL2 and/or make it easier to use.
  (sdl2::sdl-init (logior
                   sdl2-ffi:+SDL-INIT-TIMER+
                   sdl2-ffi:+SDL-INIT-AUDIO+
                   sdl2-ffi:+SDL-INIT-VIDEO+
                   sdl2-ffi:+SDL-INIT-JOYSTICK+
                   sdl2-ffi:+SDL-INIT-HAPTIC+
                   sdl2-ffi:+SDL-INIT-GAMECONTROLLER+
                   sdl2-ffi:+SDL-INIT-EVENTS+
                   sdl2-ffi:+SDL-INIT-SENSOR+))

  (let* ((refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))
         (resolution (v2:vec* =window-width= =window-height=))
         (display (make-instance 'display
                                 :window (make-window)
                                 :refresh-rate refresh-rate
                                 :resolution resolution)))
    ;; opengl context must be created AFTER SDL window creation.
    (make-opengl-context display)
    (sdl2:gl-set-swap-interval (if =vsync= 1 0))
    ;;(format t "SDL Swap interval is: ~A~%" (sdl2:gl-get-swap-interval))
    (if =allow-screensaver=
        (sdl2:enable-screensaver)
        (sdl2:disable-screensaver))
    (setf (slot-value core '%display) display)))

(defun kill-display (core)
  (u:when-let ((display (display core)))
    (sdl2:gl-delete-context (context display))
    (sdl2:destroy-window (window display)))
  ;; NOTE: Same issue with sdl2:quit as with sdl2:sdl-init above...
  (sdl2:sdl-quit))

;; TODO: The functions below are not finalized yet. The rendering pipeline need
;; to be reworked first. ~axion 4/8/2020

(defun clear-screen (core)
  (multiple-value-call #'gl:clear-color
    (if (eq =log-level= :debug)
        (values (* 0.25 (abs (sin (total-time (context core))))) 0 0 1)
        (values 0 0 0 1)))
  (gl:clear :color-buffer :depth-buffer))


;; TODO: Candidate for profiling utilities in vutils.
(defun frame-profile-result-func (val mod-val strm fmt &rest vals)
  (lambda (ms)
    (when (zerop (mod val mod-val))
      (apply #'format strm (concatenate 'string "[took ~,5F ms]: " fmt) ms
             vals))))

(defun frame-profile-nop-func (val mod-val strm fmt &rest vals)
  (declare (ignore val mod-val strm fmt vals))
  (lambda (ms)
    (declare (ignore ms))
    nil))

;; TODO: Candidate for profiling utilities in vutils.
(defmacro with-time-profile-body (time-func &body body)
  (u:with-gensyms (mv-result start-sec start-ms end-sec end-ms)
    `(u:mvlet ((,start-sec ,start-ms (sb-ext:get-time-of-day)))
       (let ((,mv-result (multiple-value-list (progn ,@body))))
         (u:mvlet ((,end-sec ,end-ms (sb-ext:get-time-of-day)))
           (funcall ,time-func (* (- (+ ,end-sec (/ ,end-ms 1d6))
                                     (+ ,start-sec (/ ,start-ms 1d6)))
                                  1d3))
           (values-list ,mv-result))))))

(defun render-frame (core)
  (with-slots (%clock %display %running-p) core
    (when %running-p
      ;; TODO: There should be a proper statistics object (the we can
      ;; turn off during production runs) to keep all this stuff from
      ;; all over the engine.

      (with-time-profile-body
          (frame-profile-nop-func ;; Try frame-profile-result-func...
           (clock-frame-count %clock)
           60
           t
           "Frame ~A rendered.~%"
           (clock-frame-count %clock))

        ;; The Body
        (clear-screen core)
        (execute-flow core
                      :default
                      'perform-one-frame
                      'entry/perform-one-frame
                      :come-from-state-name :ef)
        (sdl2:gl-swap-window (window %display))
        (incf (clock-frame-count %clock)))

      (when *profile*
        (incf =profile-frame-counter=)))))

(defun get-resolution (context)
  (resolution (display (core context))))
