(in-package #:colony)

(defun load-initial-scene (core scene-name)
  (let ((scene-name (or scene-name =initial-scene=)))
    (make-prefab-instance core scene-name)))

(defun initialize (core scene-name init-only)
  (setup-repl)
  (u:initialize-rng)
  (prepare-gamepads)
  (make-display core)
  (make-input-data core)
  (load-hardware-info)
  (load-graphs core)
  (load-call-flows core)
  (initialize-shaders core)
  (texmaptab:reify-texture-map-descriptors core)
  (tex:reify-texture-profiles core)
  (tex:reify-texture-descriptors core)
  (load-materials core)
  (initialize-collider-system core)
  (make-scene-tree core)
  (load-initial-scene core scene-name)
  ;; TODO: Right here, eagerly load any known resources the scene needs
  (run-prologue core)

  ;; BEGIN DEBUGGING
  ;; Ok, let's see what happens when we attempt to materialize a texture-map
  ;; into the resource-cache.
  ;; working ones:
  ;; colony-examples:1d-gradient
  ;; colony-examples:blue-fur (2d contains mipmaps)
  ;; colony-examples:3d (all slices loaded)
  ;; colony-examples:cube-map :faces
  (texmap::materialize
   core (list
         (u:format-symbol "COLONY-EXAMPLES" "~A"
                          "1D-GRADIENT-MIPMAPS-VERTICAL-TOP-LEFT-BIG")
         #++(u:format-symbol "COLONY-EXAMPLES" "~A" "BLUE-FUR")
         #++(u:format-symbol "COLONY-EXAMPLES" "~A" "3D")
         #++(u:format-symbol "COLONY-EXAMPLES" "~A" "CUBE-MAP")
         ))
  ;; END DEBUGGING

  (when init-only
    (format t "Stopping after initialization because INIT-ONLY is true!~%")
    (return-from initialize t))

  ;; TODO: In the game loop frame execution, we lazily load anything we need
  ;; when the app attempts to observe/add the asset data.
  (start-game-loop core))

(defun deinitialize (core)
  (run-epilogue core)
  (shutdown-gamepads core)
  (kill-display core)
  (tpool:destroy-thread-pool (thread-pool core))
  (setf *core-debug* *no-core-value*))

(defun start-game-loop (core)
  ;; Note: We let-bind the following variables so that we don't have to
  ;; dynamically access core's slot values each step of the main game loop.
  (let ((context (context core))
        (input-data (input-data core)))
    (with-profiling core
      (u:while (running-p core)
        (with-continuable
          (handle-events input-data)
          (render-frame core)
          ;; TODO: Remove this later when possible.
          (when (on-button-enter context :key :escape)
            (stop core)))))))

(defun start (&key config scene init-only)
  (load-config config)
  (let ((core (make-core config)))
    (unwind-protect (initialize core scene init-only)
      (deinitialize core))))

(defun stop (core)
  (setf (running-p core) nil))

(defun inspect-core ()
  (inspect *core-debug*))
