(in-package #:first-light.example)

;;; Prefabs

(fl:define-material dynamic-geometry
  (:profiles (fl.materials:u-mvp)
   :shader fl.shader.user:dynamic-geometry))

(fl:define-prefab "dynamic-geometry" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("plane")
   (fl.comp:transform :rotate/inc (q:orient :local (v3:one) pi)
                      :scale (v3:make 10 10 10))
   (fl.comp:dynamic-mesh :geometry '%fl::cell)
   (fl.comp:render :material 'dynamic-geometry
                   :mode :dynamic-mesh)))

;;; Prefab descriptors

(fl:define-prefab-descriptor dynamic-geometry ()
  ("dynamic-geometry" fl.example:examples))
