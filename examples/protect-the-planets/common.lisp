(in-package #:first-light.examples.protect-the-planets)

(v:define-options ()
  :title "Protect the Planets"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug
  :log-repl-categories '(:v)
  ;; NOTE: Make physics compute faster for this game.
  ;;
  ;; TODO: Move into a physics specification DSL (which also does collision
  ;; layers, etc, etc, etc)
  :delta 1/120
  :initial-scene 'lgj-04/2019)

(v:define-resources (:project :first-light.example)
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
  (log:trace :changeme "Running Protect-The-Planets prologue method."))

(defun epilogue (context)
  (declare (ignore context))
  (log:trace :changeme "Running Protect-The-Planets epilogue method."))

;;; Prefabs

(v:define-prefab "cameras" (:library ptp-base)
  ("ortho"
   (comp:camera :active-p t
                :mode :orthographic))
  ("perspective"
   (comp:camera :active-p t
                :mode :perspective))
  ("iso"
   (comp:transform :rotate (q:orient :local
                                     :x (- (atan (/ (sqrt 2))))
                                     :y (- (/ pi 4))))
   ("camera"
    (comp:transform :translate (v3:vec 0 0 10))
    (comp:camera :active-p t
                 :mode :orthographic))))

(v:define-prefab "mesh" (:library ptp-base)
  (comp:static-mesh :location '((:core :mesh) "plane.glb"))
  (comp:render :material 'contrib.mat:unlit-texture))

;;; Graphs

(in-package #:virality.engine)

(define-graph :first-light.example
    (:category component-dependency
     :depends-on ((:core (all-unknown-types core-types)))
     :roots (all-ordered-types))
  (subdag all-ordered-types
          ((splice core-types)
           -> (splice all-unknown-types))))

(define-graph :virality.engine
    (:category component-package-order
     :depends-on ((:core-component-order (core-packages)))
     :roots (start-search))
  (subdag current-project (:first-light.example))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
