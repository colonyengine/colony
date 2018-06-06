(in-package :fl.core)

;; This is a toplevel variable that is treated as a true global, rather than a special variable.
;; NOTE; This is purely used for debugging, and should definitely not be used for user code or
;; running multiple core-state objects.
(defvar *core-state*)

(defgeneric prologue (context)
  (:method (context) nil))

(defgeneric epilogue (context)
  (:method (context) nil))

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

(defun set-host (core-state)
  (setf (slot-value core-state '%host) (cfg (context core-state) :host)))

(defun prepare-engine (scene-name)
  "Bring up the engine on the main thread, while keeping the REPL unblocked for interactive
development."
  (let* ((*package* (find-package :fl.core))
         (user-package (au:make-keyword (package-name (symbol-package scene-name))))
         (user-path (get-extension-path user-package))
         (core-state (make-core-state :user-package user-package)))
    (prepare-extension :settings (context core-state) user-path)
    (set-host core-state)
    (make-display core-state)
    (prepare-extensions core-state user-path)
    (load-scene core-state scene-name)
    (prepare-shader-programs core-state)
    (resolve-all-textures core-state)
    (resolve-all-materials core-state)
    (main-loop core-state)
    (run-prologue core-state)
    core-state))

(defun start-engine (scene-name)
  "Start the engine by running the specified scene."
  (setf *core-state* (prepare-engine scene-name)))

(defun stop-engine (core-state)
  "Stop the engine, making sure to call any user-defined epilogue function first, and finally
cleaning up."
  (unwind-protect
       (with-cfg (title) (context core-state)
         (run-epilogue core-state)
         (quit-display (display core-state))
         (simple-logger:emit :engine.quit title))
    (makunbound '*core-state*)))

(defun main-loop (core-state)
  (with-continue-restart
    (setf (running-p core-state) t)
    (loop
      (fl.host:handle-events (host core-state) core-state)
      (update-lisp-repl)
      (render core-state))))

#+sbcl
(defmacro profile (scene-name duration)
  "Profile the scene `SCENE-NAME` for the given `DURATION` in seconds, all packages that begin with
  'FL.', along with some key third-party library packages."
  (let ((packages (remove-if-not
                   (lambda (x) (au:string-starts-with? x "FL."))
                   (mapcar #'package-name (list-all-packages)))))
    `(let ((engine (start-engine ,scene-name)))
       (sb-profile:unprofile)
       (sb-profile:profile ,@packages "AU" "BOX.FRAME" "SHADOW" "CL-OPENGL")
       (sleep ,duration)
       (sb-profile:report)
       (sb-profile:unprofile)
       (sb-profile:reset)
       (stop-engine engine))))
