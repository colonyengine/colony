(in-package :fl.core)

(defclass display (box.frame:frame-manager)
  ((%core-state :reader core-state
                :initarg :core-state)
   (%window :reader window
            :initarg :window)
   (%hz :reader hz
        :initarg :hz)))

(defgeneric make-display (core-state)
  (:method ((core-state core-state))
    (with-cfg (host vsync title window-width window-height delta periodic-interval debug-interval)
        (context core-state)
      (fl.host:initialize-host host)
      (setup-lisp-repl)
      (let* ((window (fl.host:create-window host title window-width window-height))
             (hz (fl.host:get-refresh-rate host window)))
        (fl.host:create-opengl-context host window 4 3)
        (setf (slot-value core-state '%display)
              (make-instance 'display
                             :window window
                             :vsyncp vsync
                             :core-state core-state
                             :hz hz
                             :delta delta
                             :period periodic-interval
                             :debug-interval debug-interval))
        (simple-logger:emit :display.init window-width window-height hz)))))

(defmethod make-display :after ((core-state core-state))
  (with-cfg (host gl-capabilities gl-blend-mode gl-depth-mode vsync) (context core-state)
    (apply #'gl:enable gl-capabilities)
    (apply #'gl:blend-func gl-blend-mode)
    (gl:depth-func gl-depth-mode)
    (fl.host:set-draw-mode host (if vsync :sync :immediate))))

(defmethod clear-screen ((display display))
  (let ((elapsed-time (box.frame:total-time display)))
    (multiple-value-call #'gl:clear-color
      (if (debug-p (context (core-state display)))
          (values (* 0.2 (abs (sin elapsed-time))) 0 0 1)
          (values 0 0 0 1)))
    (gl:clear :color-buffer :depth-buffer)))

(defun render (core-state)
  (with-slots (%host %running-p %display) core-state
    (when (and %running-p %display)
      (clear-screen %display)
      (execute-flow core-state
                    :default
                    'perform-one-frame
                    'entry/perform-one-frame
                    :come-from-state-name :ef)
      (fl.host:redraw-window %host (window %display)))))

(defmethod quit-display ((display display))
  (with-slots (%core-state %window) display
    (let ((host (host %core-state)))
      (unwind-protect
           (progn
             (fl.host:close-window host %window)
             (fl.host:shutdown-host host))
        (setf (running-p %core-state) nil)))))
