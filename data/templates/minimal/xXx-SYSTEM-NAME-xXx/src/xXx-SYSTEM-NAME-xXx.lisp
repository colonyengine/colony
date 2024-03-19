(in-package #:xXx-SYSTEM-NAME-xXx)

;;; Fundamental Prefabs

(c:define-prefab "cameras" (:library lib/main)
  ("ortho"
   ;;(comp:transform :translate (v3:vec 0f0 0f0 10f0))
   (comp:camera :active-p t
                :mode :orthographic))
  ("perspective"
   (comp:transform :translate (v3:vec 0f0 0f0 10f0))
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

(c:define-prefab "mesh" (:library lib/main)
  (comp:mesh :asset '(c::meshes c::primitives)
             :name "plane")
  (comp:render :material 'x:unlit-texture
               :slave (c:ref :self :component 'comp:mesh)))



;;; This acts like a scene, but is just a regular prefab. 
 
(c:define-prefab "initial-scene" (:library lib/main)
  (("camera" :copy "/cameras/perspective")
   (comp:transform :translate (v3:vec 0f0 0f0 5f0))
   (comp:camera :zoom 3f0))
  (("mesh" :copy "/mesh")))

