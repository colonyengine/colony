(in-package :gear)

(defun prepare-engine (package)
  (let ((*package* (find-package :gear))
        (core-state (make-instance 'core-state))
        (path (get-path package "data")))
    (prepare-extensions core-state path)
    (load-default-scene core-state)
    (make-display core-state)
    (compile-shaders core-state)
    core-state))

(defun start-engine ()
  (let* ((user-package-name (make-keyword (package-name *package*))))
    (when (eq user-package-name :gear)
      (error "Cannot start the engine from the :GEAR package."))
    (kit.sdl2:init)
    (prog1
        (sdl2:in-main-thread ()
          (prepare-engine user-package-name))
      (kit.sdl2:start))))

#+sbcl
(defmacro profile (seconds)
  `(progn
     (let ((display (display (start-engine))))
       (sb-profile:unprofile)
       (sb-profile:profile
        "GEAR"
        "GEAR-EXAMPLE"
        "GAMEBOX-MATH"
        "GAMEBOX-FRAME-MANAGER")
       (sleep ,seconds)
       (sb-profile:report)
       (sb-profile:unprofile)
       (sb-profile:reset)
       (quit-engine display))))
