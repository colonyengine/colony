(in-package :%fl)

(au:eval-always
  (defmacro profile (core-state duration)
    "Profile the scene `SCENE-NAME` for the given `DURATION` in seconds, all packages that begin with
  'FL.', along with some key third-party library packages."
    (let ((packages (remove-if-not
                     (lambda (x) (au:string-starts-with-p x "FL."))
                     (mapcar #'package-name (list-all-packages)))))
      `(progn
         (sb-profile:unprofile)
         (sb-profile:profile ,@packages "AU" "SHADOW" "GL" "%GL")
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
  (setf (state (context core-state)) (prologue (context core-state))))

(defun run-epilogue (core-state)
  "The epilogue is a (defmethod epilogue ((context context)) ..) defined in the user's project. If
it exists, it is called after the last frame, and after the last invocations of any component protocol
method, but before any engine tear-down procedure occurs when stopping the engine."
  (epilogue (context core-state)))

(defun initialize-host ()
  (let ((flags '(:everything)))
    (unless (apply #'sdl2:was-init flags)
      (let ((flags (autowrap:mask-apply 'sdl2::sdl-init-flags flags)))
        (sdl2::check-rc (sdl2::sdl-init flags))))))

(defmethod %initialize-engine :before ((core-state core-state) scene-name)
  (with-slots (%user-package %data-path %context %settings) core-state
    (setf %user-package (get-scene-package scene-name)
          %data-path (get-extension-path %user-package))
    (prepare-extension :settings core-state)
    (setf *core-state-debug* core-state
          %context (make-instance 'context :core-state core-state :settings %settings)
          simple-logger:*current-level* (cfg %context :log-level))))

(defmethod %initialize-engine ((core-state core-state) scene-name)
  (setup-lisp-repl)
  (initialize-host)
  (prepare-gamepads)
  (make-display core-state)
  (prepare-extension :graphs core-state)
  (prepare-extension :call-flow core-state)
  (prepare-extension :shader-stages core-state)
  (prepare-extension :shader-programs core-state)
  (prepare-extension :textures core-state)
  (prepare-extension :materials core-state)
  (prepare-extension :scene core-state)
  (prepare-shader-programs core-state)
  (resolve-all-textures core-state)
  (resolve-all-materials core-state)
  (load-scene core-state scene-name))

(defun iterate-main-loop (core-state)
  (with-continue-restart "First Light"
    (handle-events core-state)
    (render core-state)))

(defun main-loop (core-state)
  (au:while (running-p core-state)
    (iterate-main-loop core-state)))

(defun start-engine (scene-name &key (profile 0))
  "Start the engine. First we initialize the engine. Next we run the prologue as the last step,
before finally starting the main game loop."
  (let ((core-state (make-instance 'core-state :running-p t)))
    (%initialize-engine core-state scene-name)
    (run-prologue core-state)
    (if (and (numberp profile) (plusp profile))
        (profile core-state profile)
        (main-loop core-state))))

(defun stop-engine (core-state)
  "Stop the engine, making sure to call any user-defined epilogue function first, and finally
cleaning up."
  (run-epilogue core-state)
  (shutdown-shader-programs)
  (shutdown-gamepads core-state)
  (quit-display (display core-state))
  (setf (running-p core-state) nil)
  (makunbound '*core-state-debug*)
  (simple-logger:emit :engine.quit (cfg (context core-state) :title)))
