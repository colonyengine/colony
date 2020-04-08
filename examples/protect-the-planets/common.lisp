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
                                        :x (float (- (atan (/ (sqrt 2)))) 1f0)
                                        :y (- o:pi/4)))
   ("camera"
    (c/xform:transform :translate (v3:vec 0f0 0f0 10f0))
    (c/cam:camera :active-p t
                  :mode :orthographic))))

(v:define-prefab "mesh" (:library ptp-base)
  (c/smesh:static-mesh :asset '(:virality.engine/mesh "plane.glb"))
  (c/render:render :material 'x/mat:unlit-texture))
