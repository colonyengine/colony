(in-package #:virality.examples)

(v:define-options :virality.examples
  :window-width 1920
  :window-height 1080
  :vsync :off
  :delta 1/120
  :initial-scene 'geometric-volumes)

(v:define-assets :virality.examples
  :data "data"
  :mesh ("mesh" :data)
  :tex ("texture" :data)
  :log "log"
  :log-debug ("debug.log" :log)
  :log-error ("error.log" :log)
  :texture-example ("texture" :tex)
  :1da ("1d-array" :texture-example)
  :2da ("2d-array" :texture-example)
  :3d ("3d" :texture-example)
  :cubemap ("cube-map" :texture-example)
  :cubemaparray ("cube-map-array" :texture-example)
  :spritesheet ("sprite/sprites.tiff" :tex)
  :spritesheet-data ("sprites.spec" :data)
  :damaged-helmet-textures ("damaged-helmet" :tex)
  :ptp-tex ("protect-the-planets" :tex))

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
