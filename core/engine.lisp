(in-package :%first-light)

(au:eval-always
  (defmacro profile (core-state duration)
    "Profile the scene `SCENE-NAME` for the given `DURATION` in seconds, all packages that begin with
  'FL.', along with some key third-party library packages."
    (let ((packages (remove-if-not
                     (lambda (x) (au:string-starts-with-p x "FL."))
                     (mapcar #'package-name (list-all-packages)))))
      `(progn
         (sb-profile:unprofile)
         (sb-profile:profile ,@packages)
         (au:while (and (running-p core-state)
                        (<= (total-time (context ,core-state)) ,duration))
           (iterate-main-loop ,core-state))
         (when (running-p core-state)
           (stop-engine ,core-state))
         (sb-profile:report)
         (sb-profile:unprofile)
         (sb-profile:reset)))))

(defgeneric prologue (context)
  (:method (context)
    (au:noop)))

(defgeneric epilogue (context)
  (:method (context)
    (au:noop)))

(defun run-prologue (core-state)
  "The prologue is a (defmethod prologue ((context context)) ...) method optionally defined in the
user's project. If it exists, it is called after internal engine setup, but before the first
frame (specifically, before any component protocol methods are called for the first time). The
result of the prologue function is automatically stored in the STATE slot of the CONTEXT."
  (let ((context (context core-state)))
    (setf (state context) (prologue context))))

(defun run-epilogue (core-state)
  "The epilogue is a (defmethod epilogue ((context context)) ..) defined in the user's project. If
it exists, it is called after the last frame, and after the last invocations of any component protocol
method, but before any engine tear-down procedure occurs when stopping the engine."
  (epilogue (context core-state)))

(defun initialize-host (core-state)
  (let ((flags '(:everything))
        (gamepad-db (find-resource (context core-state) '(:core :gamepad-db))))
    (unless (apply #'sdl2:was-init flags)
      (let ((flags (autowrap:mask-apply 'sdl2::sdl-init-flags flags)))
        (sdl2::check-rc (sdl2::sdl-init flags))))
    (fl.input:prepare-gamepads gamepad-db)
    (make-display core-state)))

(defun shutdown-host (core-state)
  (fl.input:shutdown-gamepads (input-data core-state))
  (sdl2:destroy-window (window (display core-state)))
  (sdl2::sdl-quit))

(defmethod initialize-engine ((core-state core-state) scene-name)
  (let ((title (option (context core-state) :title)))
    (v:info :fl.core.engine "Starting up ~a..." title)
    (setup-lisp-repl)
    (enable-logging core-state)
    (make-frame-manager core-state)
    (initialize-host core-state)
    (initialize-shaders core-state)
    (load-graphs core-state)
    (load-call-flows core-state)
    (load-texture-descriptors core-state)
    (load-materials core-state)
    (load-scene-definitions core-state)
    (load-scene core-state scene-name)
    (v:info :fl.core.engine "Finished starting ~a" title)))

(defun iterate-main-loop (core-state)
  (with-continue-restart "First Light"
    (fl.input:handle-events (input-data core-state))
    (render core-state)
    ;; TODO: Remove this later when possible.
    (when (fl.input:input-enter-p (input-data core-state) '(:key :escape))
      (stop-engine core-state))))

(defun main-loop (core-state)
  (initialize-frame-time core-state)
  (au:while (running-p core-state)
    (iterate-main-loop core-state)))

(defun start-engine (scene-name &optional profile-duration)
  "Start the engine. First we initialize the engine. Next we run the prologue as the last step,
before finally starting the main game loop."
  (unwind-protect
       (let ((core-state (make-instance 'core-state)))
         (load-options core-state)
         (make-context core-state)
         (setf *core-state-debug* core-state)
         (initialize-engine core-state scene-name)
         (run-prologue core-state)
         (if profile-duration
             (profile core-state profile-duration)
             (main-loop core-state)))
    (sdl2::sdl-quit)))

(defun stop-engine (core-state)
  "Stop the engine, making sure to call any user-defined epilogue function first, and finally
cleaning up."
  (let ((title (option core-state :title)))
    (v:info :fl.core.engine "Shutting down ~a..." title)
    (run-epilogue core-state)
    (fl.gpu:unload-shaders)
    (shutdown-host core-state)
    (setf (running-p core-state) nil)
    (makunbound '*core-state-debug*)
    (v:info :fl.core.engine "Successfully shut down ~a" title)))
