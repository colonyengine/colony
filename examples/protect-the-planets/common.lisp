(in-package #:virality.examples)

;;; Prefabs

(v:define-prefab "cameras" (:library ptp-base)
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

(v:define-prefab "mesh" (:library ptp-base)
  (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
  (c/render:render :material 'x/mat:unlit-texture))
