(in-package #:virality-examples)

(v:define-config :virality-examples ()
  :window-width 1280
  :window-height 720
  :delta (float 1/120 1f0)
  :initial-scene '(("geometric-volumes" examples)))

(v:define-asset-pool metadata ()
  :path "data/metadata")

(v:define-asset-pool meshes ()
  :path "data/mesh"
  :filter "glb")

(v:define-asset-pool textures ()
  :path "data/texture"
  :filter "png")

(v:define-asset-pool mesh-textures ()
  :path "data/texture/mesh"
  :filter "png")

(v:define-asset-pool ptp-textures ()
  :path "data/texture/protect-the-planets"
  :filter "png")

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
                                     :x (- (atan (/ (sqrt 2f0))))
                                     :y (- o:pi/4)))
   ("camera"
    (comp:transform :translate (v3:vec 0f0 0f0 10f0))
    (comp:camera :active-p t
                 :mode :orthographic))))

(v:define-prefab "mesh" (:library examples)
  (comp:mesh :asset '(v::meshes v::primitives)
             :name "plane")
  (comp:render :material 'x/mat:unlit-texture))

;;; Graphs

;; TODO: FIgure out why the graph DSL can't parse syntax based on symbol-name.
;; The following in-package form is needed until this is fixed

(in-package #:virality)

(define-graph :virality-examples
    (:category component-dependency
     :depends-on ((:core (all-unknown-types core-types)))
     :roots (all-ordered-types))
  (subdag all-ordered-types
          ((splice core-types)
           -> (splice all-unknown-types))))

(define-graph :virality
    (:category component-package-order
     :depends-on ((:core-component-order (core-packages)))
     :roots (start-search))
  (subdag current-project (:comp -> :virality-examples))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
