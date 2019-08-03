(in-package #:virality.examples)

;; Dynamic geometry

(v:define-geometry-layout tile
  (:data (:format interleaved)
         (position :type float :count 3)
         (normal :type float :count 3)
         (uv :type float :count 3)))

(v:define-geometry tile
  :layout tile
  :primitive :triangle-strip
  :vertex-count 4
  :buffers
  (:data (((-0.5 0.5 0) (0 0 1) (-1 1 0))
          ((-0.5 -0.5 0) (0 0 1) (-1 -1 0))
          ((0.5 0.5 0) (0 0 1) (1 1 0))
          ((0.5 -0.5 0) (0 0 1) (1 -1 0)))))

;;; Prefabs

(v:define-material dynamic-geometry
  (:profiles (contrib.mat:u-mvp)
   :shader first-light.shader.user:dynamic-geometry))

(v:define-prefab "dynamic-geometry" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("plane")
   (comp:transform :rotate/inc (q:orient :local :x pi)
                   :scale (v3:vec 20 20 20))
   (comp:dynamic-mesh :geometry 'tile)
   (comp:render :material 'dynamic-geometry
                :mode :dynamic-mesh)))

;;; Prefab descriptors

(v:define-prefab-descriptor dynamic-geometry ()
  ("dynamic-geometry" examples))
