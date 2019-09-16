(in-package #:virality.examples)

(v:define-options :virality.examples
  :window-width 1280
  :window-height 720
  :vsync :off
  :initial-scene 'geometric-volumes)

(v:define-assets :virality.examples
  :data "data"
  :mesh (:data "mesh")
  :tex (:data "texture")
  :log "log"
  :log-debug (:log "debug.log")
  :log-error (:log "error.log")
  :texture-example (:tex "texture")
  :1da (:texture-example "1d-array")
  :2da (:texture-example "2d-array")
  :3d (:texture-example "3d")
  :cubemap (:texture-example "cube-map")
  :cubemaparray (:texture-example "cube-map-array")
  :playground-tex (:tex "example-playground")
  :spritesheet (:tex "sprite/sprites.tiff")
  :spritesheet-data (:data "sprites.spec")
  :damaged-helmet-textures (:tex "damaged-helmet")
  :ptp-tex (:tex "protect-the-planets"))

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
  (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
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
