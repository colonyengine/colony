(in-package #:first-light.example)

(fl:define-options ()
  :title "First-Light Examples"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug
  :log-repl-categories '(:fl)
  ;; NOTE: Make physics compute faster as fast for these examples.
  ;; This is really here because of the lisp game jam april 2019 codes.
  :delta 1/120
  :initial-scene 'geometric-volumes)

(fl:define-resources (:project :first-light.example)
  (:project "data/project")
  (:ext (:project "ext"))
  (:mesh (:project "mesh"))
  (:texture (:project "texture"))
  (:lgj-04/2019 (:project :texture "lisp-game-jam-04-2019"))
  (:log (:project "log"))
  (:log-debug (:project :log "debug.log"))
  (:log-error (:project :log "error.log"))
  (:example-texture (:project :texture "example-texture"))
  (:1da (:project :example-texture "1d-array"))
  (:2da (:project :example-texture "2d-array"))
  (:3d (:project :example-texture "3d"))
  (:cubemap (:project :example-texture "cube-map"))
  (:cubemaparray (:project :example-texture "cube-map-array"))
  (:spritesheet (:project :texture "example-sprite/sprites.tiff"))
  (:spritesheet-data (:project "sprites.sexp"))
  (:damaged-helmet-textures (:project :texture "example-damaged-helmet")))

(defun prologue (context)
  (declare (ignore context))
  (v:trace :fl.core.engine "Running prologue method."))

(defun epilogue (context)
  (declare (ignore context))
  (v:trace :fl.core.engine "Running epilogue method."))

;;; Prefabs

(fl:define-prefab "cameras" (:library examples)
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

(fl:define-prefab "mesh" (:library examples)
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
