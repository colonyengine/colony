(in-package #:virality.examples.protect-the-planets)

(v:define-options ()
  :title "Protect the Planets"
  :window-width 1920
  :window-height 1080
  :vsync :off
  ;; NOTE: Make physics compute faster for this game.
  ;;
  ;; TODO: Move into a physics specification DSL (which also does collision
  ;; layers, etc, etc, etc)
  :delta 1/120)

(v:define-resources (:project :virality.examples)
  ;; TODO: Move this into new location once changing to tbe new package is done.
  (:project "protect-the-planets/data")
  (:texture (:project "texture"))
  (:sprite (:project "sprite-sheet"))
  (:log (:project "log"))
  (:log-debug (:project :log "debug.log"))
  (:log-error (:project :log "error.log"))
  (:spritesheet (:project :sprite "sprites.tiff"))
  (:spritesheet-data (:project :sprite "sprites.sexp")))

;;; Prefabs

(v:define-prefab "cameras" (:library ptp-base)
  ("ortho"
   (comp.camera:camera :active-p t
                       :mode :orthographic))
  ("perspective"
   (comp.camera:camera :active-p t
                       :mode :perspective))
  ("iso"
   (comp.transform:transform :rotate (q:orient :local
                                               :x (- (atan (/ (sqrt 2))))
                                               :y (- (/ pi 4))))
   ("camera"
    (comp.transform:transform :translate (v3:vec 0 0 10))
    (comp.camera:camera :active-p t
                        :mode :orthographic))))

(v:define-prefab "mesh" (:library ptp-base)
  (comp.mesh.static:static-mesh :location '((:core :mesh) "plane.glb"))
  (comp.render:render :material 'contrib.mat:unlit-texture))

;;; Graphs

;; TODO: FIgure out why the graph DSL can't parse syntax based on symbol-name.
;; The following in-package form is needed until this is fixed

(in-package #:virality.engine)

(define-graph :virality.examples.protect-the-planets
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
  (subdag current-project (:virality.examples))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
