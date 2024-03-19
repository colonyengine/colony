(in-package #:colony-examples)

;;; Prefabs

(c:define-prefab "cameras" (:library ptp-base)
  ("ortho"
   (comp:camera :active-p t
                :mode :orthographic))
  ("perspective"
   (comp:camera :active-p t
                :mode :perspective))
  ("iso"
   (comp:transform :rotate (q:orient :local
                                     :x (float (- (atan (/ (sqrt 2)))) 1f0)
                                     :y (- o:pi/4)))
   ("camera"
    (comp:transform :translate (v3:vec 0f0 0f0 10f0))
    (comp:camera :active-p t
                 :mode :orthographic))))

(c:define-prefab "mesh" (:library ptp-base)
  (comp:mesh :asset '(c::meshes c::primitives)
             :name "plane")
  (sketch :material 'x:unlit-texture
          :slave (c:ref :self :component 'comp:mesh)))
