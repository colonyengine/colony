(in-package #:colony-examples)

(c:define-config colony-examples ()
  :default t
  :window-width 1920 ;; 1280
  :window-height 1080 ;; 720
  :delta (float 1/120 1f0)
  :initial-scene '(("example-selector" examples)))

(c:define-asset-pool metadata ()
  :path "data/metadata")

(c:define-asset-pool meshes ()
  :path "data/mesh"
  :filter "glb")

(c:define-asset-pool textures ()
  :path "data/texture"
  :filter "png")

(c:define-asset-pool mesh-textures ()
  :path "data/texture/mesh"
  :filter "png")

(c:define-asset-pool environments ()
  :path "data/texture/environment"
  :filter "hdr")

(c:define-asset-pool ptp-textures ()
  :path "data/texture/protect-the-planets"
  :filter "png")

;;; Prefabs

(c:define-prefab "cameras" (:library examples)
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

(c:define-prefab "mesh" (:library examples)
  (comp:mesh :asset '(c::meshes c::primitives)
             :name "plane")
  (comp:render :material 'x:unlit-texture
               :slave (c:ref :self :component 'comp:mesh)))

;;; Graphs

;; TODO: FIgure out why the graph DSL can't parse syntax based on symbol-name.
;; The following in-package form is needed until this is fixed

(in-package #:colony)

(define-graph :colony-examples
    (:category component-dependency
     :depends-on ((:core (all-unknown-types core-types)))
     :roots (all-ordered-types))
  (subdag all-ordered-types
          ((splice core-types)
           -> colony-examples::sketch
           -> colony-examples::delayed-render
           -> (splice all-unknown-types))))

(define-graph :colony
    (:category component-package-order
     :depends-on ((:core-component-order (core-packages)))
     :roots (start-search))
  (subdag current-project (:comp -> :colony-examples))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
