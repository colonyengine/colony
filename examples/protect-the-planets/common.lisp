(in-package #:first-light.examples.protect-the-planets)

(fl:define-options ()
  :title "Protect the Planets"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug
  :log-repl-categories '(:fl)
  ;; NOTE: Make physics compute faster for this game.
  ;;
  ;; TODO: Move into a physics specification DSL (which also does collision
  ;; layers, etc, etc, etc)
  :delta 1/120
  :initial-scene 'lgj-04/2019)

(fl:define-resources (:project :first-light.example)
  ;; TODO: Move this into new location once changing to tbe new package is done.
  (:project "protect-the-planets/data")
  (:texture (:project "texture"))
  (:sprite (:project "sprite-sheet"))
  (:log (:project "log"))
  (:log-debug (:project :log "debug.log"))
  (:log-error (:project :log "error.log"))

  (:spritesheet (:project :sprite "sprites.tiff"))
  (:spritesheet-data (:project :sprite "sprites.sexp"))
  )

(defun prologue (context)
  (declare (ignore context))
  (log:trace :fl.core.engine "Running Protect-The-Planets prologue method."))

(defun epilogue (context)
  (declare (ignore context))
  (log:trace :fl.core.engine "Running Protect-The-Planets epilogue method."))

;;; Prefabs

(fl:define-prefab "cameras" (:library ptp-base)
  ("ortho"
   (fl.comp:camera :active-p t
                   :mode :orthographic))
  ("perspective"
   (fl.comp:camera :active-p t
                   :mode :perspective))
  ("iso"
   (fl.comp:transform :rotate (q:orient :local
                                        :x (- (atan (/ (sqrt 2))))
                                        :y (- (/ pi 4))))
   ("camera"
    (fl.comp:transform :translate (v3:vec 0 0 10))
    (fl.comp:camera :active-p t
                    :mode :orthographic))))

(fl:define-prefab "mesh" (:library ptp-base)
  (fl.comp:static-mesh :location '((:core :mesh) "plane.glb"))
  (fl.comp:render :material 'fl.materials:unlit-texture))

;;; Graphs

;;; TODO: Fix graphs to work in user package
(in-package #:%first-light)

(fl:define-graph :fl.example
    (:category component-dependency
     :depends-on ((:core (all-unknown-types core-types)))
     :roots (all-ordered-types))
  (subdag all-ordered-types
          ((splice core-types)
           -> (splice all-unknown-types))))

(fl:define-graph :fl
    (:category component-package-order
     :depends-on ((:core-component-order (core-packages)))
     :roots (start-search))
  (subdag current-project (:fl.example.comp.* -> :fl.example))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
