(in-package :fl.core)

(defun prepare-engine (package)
  (let ((*package* (find-package :fl.core))
        (core-state (make-core-state :user-package package)))
    (shadow:initialize-shaders)
    (prepare-extensions core-state (get-extension-path package))
    (load-default-scene core-state)
    (make-display core-state)
    (prepare-shader-programs core-state)
    (resolve-all-materials core-state)
    core-state))

(defun prepare-shader-programs (core-state)
  (setf (shaders core-state) (shadow:build-shader-dictionary)))

(defun start-engine ()
  (let ((user-package-name (alexandria:make-keyword (package-name *package*))))
    (when (eq user-package-name :fl.core)
      (error "Cannot start the engine from the :FL.CORE package."))
    (kit.sdl2:init)
    (prog1 (sdl2:in-main-thread () (prepare-engine user-package-name))
      (kit.sdl2:start))))

#+sbcl
(defmacro profile (seconds)
  `(progn
     (let ((display (display (start-engine))))
       (sb-profile:unprofile)
       (sb-profile:profile
        "FIRST-LIGHT"
        "FIRST-LIGHT-EXAMPLE"
        "GAMEBOX-FRAME-MANAGER")
       (sleep ,seconds)
       (sb-profile:report)
       (sb-profile:unprofile)
       (sb-profile:reset)
       (quit-engine display))))
