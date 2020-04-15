(in-package #:virality)

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
           (stop ,core))
         (sb-profile:report)
         (sb-profile:unprofile)
         (sb-profile:reset)))))

(defmethod prologue :before ((context context))
  ;; TODO: I removed the verbose logging framework because it is buggy.
  ;; ~axion ;; 4/9/2020.
  #++(:printv :virality "Running prologue method."))

(defmethod prologue ((context context)))

(defmethod epilogue :before ((context context))
  ;; TODO: I removed the verbose logging framework because it is buggy.
  ;; ~axion ;; 4/9/2020.
  #++(:printv :virality "Running epilogue method."))

(defmethod epilogue ((context context)))

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

(defun load-initial-scene (core scene-name)
  (let ((scene-name (or scene-name =initial-scene=)))
    (make-prefab-instance core scene-name)))

(defun initialize-engine (core scene-name)
  (setup-repl)
  (make-display core)
  (prepare-gamepads core)
  (make-clock core)
  (initialize-shaders core)
  (make-input-data core)
  (load-hardware-info)
  (make-thread-pool)
  (load-graphs core)
  (load-call-flows core)
  (tex::load-texture-descriptors core)
  (mat::load-materials core)
  (col::initialize-collider-system core)
  (make-scene-tree core)
  (load-initial-scene core scene-name))

(defun iterate-main-loop (core)
  (with-continuable "Virality Engine"
    (let ((context (context core)))
      (handle-events core)
      (render-frame core)
      ;; TODO: Remove this later when possible.
      (when (on-button-enter context :key :escape)
        (stop core)))))

(defun main-loop (core)
  (initialize-frame-time (clock core))
  (u:while (running-p core)
    (iterate-main-loop core)))

(defun start (&key project scene profile)
  "Start the engine. First we initialize the engine. Next we run the prologue as
the last step, before finally starting the main game loop."
  (unwind-protect
       (let ((core (make-instance 'core :project project)))
         (load-config project)
         (make-context core)
         (setf *core-debug* core)
         (initialize-engine core scene)
         (run-prologue core)
         (if profile
             (profile core profile)
             (main-loop core)))
    (sdl2::sdl-quit)))

(defun stop (core)
  "Stop the engine, making sure to call any user-defined epilogue function
first, and finally cleaning up."
  (run-epilogue core)
  (gpu:unload-shaders)
  (kill-display core)
  (shutdown-gamepads core)
  (setf (running-p core) nil)
  (makunbound '*core-debug*))
