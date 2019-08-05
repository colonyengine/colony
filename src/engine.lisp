(in-package #:virality.engine)

#+sbcl
(u:eval-always
  (defmacro profile (core duration)
    (let ((packages (remove-if-not
                     (lambda (x) (u:string-starts-with-p x "VIRALITY"))
                     (mapcar #'package-name (list-all-packages)))))
      `(progn
         (sb-profile:unprofile)
         (sb-profile:profile ,@packages)
         (u:while (and (running-p core)
                       (<= (total-time (context ,core)) ,duration))
           (iterate-main-loop ,core))
         (when (running-p core)
           (stop-engine ,core))
         (sb-profile:report)
         (sb-profile:unprofile)
         (sb-profile:reset)))))

(defgeneric prologue (context)
  (:method :before (context)
    (log:trace :virality.engine "Running prologue method."))
  (:method (context)
    (u:noop)))

(defgeneric epilogue (context)
  (:method :before (context)
    (log:trace :virality.engine "Running epilogue method."))
  (:method (context)
    (u:noop)))

(defun run-prologue (core)
  "The prologue is a (defmethod prologue ((context context)) ...) method
optionally defined in the user's project. If it exists, it is called after
internal engine setup, but before the first frame (specifically, before any
component protocol methods are called for the first time). The result of the
prologue function is automatically stored in the STATE slot of the CONTEXT."
  (let ((context (context core)))
    (setf (state context) (prologue context))))

(defun run-epilogue (core)
  "The epilogue is a (defmethod epilogue ((context context)) ..) defined in the
user's project. If it exists, it is called after the last frame, and after the
last invocations of any component protocol method, but before any engine
tear-down procedure occurs when stopping the engine."
  (epilogue (context core)))

(defun initialize-host (core)
  (let ((flags '(:everything))
        (gamepad-db (find-resource (context core) '(:core :gamepad-db))))
    (unless (apply #'sdl2:was-init flags)
      (let ((flags (autowrap:mask-apply 'sdl2::sdl-init-flags flags)))
        (sdl2::check-rc (sdl2::sdl-init flags))))
    (in::prepare-gamepads gamepad-db)
    (make-display core)))

(defun shutdown-host (core)
  (in::shutdown-gamepads (context core))
  (sdl2:destroy-window (window (display core)))
  (sdl2::sdl-quit))

(defun load-initial-scene (core scene-name)
  (let* ((scene-name (or scene-name (option (context core) :initial-scene)))
         (prefab-descriptor (prefab::find-prefab-descriptor scene-name)))
    (make-prefab-instance core prefab-descriptor)))

(defun initialize-engine (core scene-name)
  (let ((title (option (context core) :title)))
    (log:info :virality.engine "Starting up ~a..." title)
    (setup-live-coding)
    (enable-logging core)
    (make-frame-manager core)
    (initialize-host core)
    (initialize-shaders core)
    (load-graphs core)
    (load-call-flows core)
    (load-texture-descriptors core)
    (load-materials core)
    (col::initialize-collider-system core)
    (make-scene-tree core)
    (load-initial-scene core scene-name)
    (log:info :virality.engine "Finished starting ~a" title)))

(defun iterate-main-loop (core)
  (with-continue-restart "Virality Engine"
    (let ((context (context core)))
      (in::handle-events context)
      (render-frame core)
      ;; TODO: Remove this later when possible.
      (when (in:input-enter-p context '(:key :escape))
        (stop-engine core)))))

(defun main-loop (core)
  (initialize-frame-time core)
  (u:while (running-p core)
    (iterate-main-loop core)))

(defun start-engine (&key scene profile)
  "Start the engine. First we initialize the engine. Next we run the prologue as
the last step, before finally starting the main game loop."
  (unwind-protect
       (let ((core (make-instance 'core)))
         (load-options core)
         (make-context core)
         (setf *core-debug* core)
         (initialize-engine core scene)
         (run-prologue core)
         (if profile
             (profile core profile)
             (main-loop core)))
    (sdl2::sdl-quit)))

(defun stop-engine (core)
  "Stop the engine, making sure to call any user-defined epilogue function
first, and finally cleaning up."
  (let ((title (option core :title)))
    (log:info :virality.engine "Shutting down ~a..." title)
    (run-epilogue core)
    (gpu:unload-shaders)
    (shutdown-host core)
    (setf (running-p core) nil)
    (makunbound '*core-debug*)
    (log:info :virality.engine "Successfully shut down ~a" title)))
