(in-package #:first-light.example)

(v:define-options ()
  :title "Virality Engine"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug
  :log-repl-categories '(:v)
  ;; NOTE: Make physics compute faster as fast for these examples.
  ;; This is really here because of the lisp game jam april 2019 codes.
  :delta 1/120
  :initial-scene 'geometric-volumes)

(v:define-resources (:project :first-light.example)
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
  (log:trace :changeme "Running prologue method."))

(defun epilogue (context)
  (declare (ignore context))
  (log:trace :changeme "Running epilogue method."))

;;; Prefabs

(v:define-prefab "cameras" (:library examples)
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

(v:define-prefab "mesh" (:library examples)
  (comp:static-mesh :location '((:core :mesh) "plane.glb"))
  (comp:render :material 'contrib.mat:unlit-texture))

;;; Graphs

;; TODO: FIgure out why the graph DSL can't parse syntax based on symbol-name.
;; The following in-package form is needed until this is fixed

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
  (subdag current-project (:comp -> :first-light.example))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
