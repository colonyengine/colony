(in-package :first-light.example)

(fl:define-prefab "collision-1" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (fl.comp:camera (:policy :new-args) :zoom 6))
  ("rot-0-center"
   (fl.comp:transform :translate (m:vec3 -2 0 0)
                      :rotate/inc (m:vec3 0 0 pi))
   ("plane-0"
    (fl.comp:transform :translate (m:vec3 -2 0 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:collider/sphere
     :name "Player"
     :on-layer :player
     :referent (fl:ref "/collision-1/rot-0-center/plane-0"
                       :component 'fl.comp:collider/sphere)
     :center (m:vec3)
     :radius 1)
    (fl.comp:render :material '2d-wood)))
  ("rot-1-center"
   (fl.comp:transform :translate (m:vec3 2 0 0)
                      :rotate/inc (m:vec3 0 0 (- pi)))
   ("plane-1"
    (fl.comp:transform :translate (m:vec3 2 0 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:collider/sphere
     :name "Enemy"
     :on-layer :enemy
     :referent (fl:ref "/collision-1/rot-1-center/plane-1"
		       :component 'fl.comp:collider/sphere)
     :center (m:vec3)
     :radius 1)
    (fl.comp:render :material '2d-wood))))
