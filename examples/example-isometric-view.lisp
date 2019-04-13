(in-package :first-light.example)

;;; Prefabs

(fl:define-prefab "isometric-view" (:library examples)
  (("camera" :copy "/cameras/iso")
   ("transform/camera"
    (fl.comp:camera (:policy :new-args) :zoom 100)))
  (("cube1" :copy "/mesh")
   (fl.comp:transform :rotate/inc (m:vec3 (/ pi 2)))
   (fl.comp:mesh :location '((:core :mesh) "cube.glb")))
  (("cube2" :copy "/mesh")
   (fl.comp:transform :translate (m:vec3 2 0 0))
   (fl.comp:mesh :location '((:core :mesh) "cube.glb"))))

;;; Prefab descriptors

(fl:define-prefab-descriptor isometric-view ()
  ("isometric-view" fl.example:examples))
