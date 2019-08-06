(in-package #:virality.examples)

(v:define-options ()
  :window-width 1920
  :window-height 1080
  :vsync :off
  :initial-scene 'geometric-volumes)

(v:define-resources (:project :virality.examples)
  (:project "data/project")
  (:mesh (:project "mesh"))
  (:texture (:project "texture"))
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

;;; Prefabs

(v:define-prefab "cameras" (:library examples)
  ("ortho"
   (c/cam:camera :active-p t
                 :mode :orthographic))
  ("perspective"
   (c/cam:camera :active-p t
                 :mode :perspective))
  ("iso"
   (c/xform:transform :rotate (q:orient :local
                                        :x (- (atan (/ (sqrt 2))))
                                        :y (- (/ pi 4))))
   ("camera"
    (c/xform:transform :translate (v3:vec 0 0 10))
    (c/cam:camera :active-p t
                  :mode :orthographic))))

(v:define-prefab "mesh" (:library examples)
  (c/smesh:static-mesh :location '((:core :mesh) "plane.glb"))
  (c/render:render :material 'x/mat:unlit-texture))

;;; Graphs

;; TODO: FIgure out why the graph DSL can't parse syntax based on symbol-name.
;; The following in-package form is needed until this is fixed

(in-package #:virality.engine)

(define-graph :virality.examples
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
  (subdag current-project (:comp -> :virality.examples))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
