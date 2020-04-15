(in-package #:virality-examples)

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
  (:data (((-0.5f0 0.5f0 0f0) (0f0 0f0 1f0) (-1f0 1f0 0f0))
          ((-0.5f0 -0.5f0 0f0) (0f0 0f0 1f0) (-1f0 -1f0 0f0))
          ((0.5f0 0.5f0 0f0) (0f0 0f0 1f0) (1f0 1f0 0f0))
          ((0.5f0 -0.5f0 0f0) (0f0 0f0 1f0) (1f0 -1f0 0f0)))))

;;; Prefabs

(v:define-material dynamic-geometry
  (:profiles (x/mat:u-mvp)
   :shader ex/shd:dynamic-geometry))

(v:define-prefab "dynamic-geometry" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("plane")
   (comp:transform :rotate/velocity (o:make-velocity v3:+right+ o:pi)
                   :scale (v3:vec 20f0 20f0 20f0))
   (comp:dynamic-mesh :geometry 'tile)
   (comp:render :material 'dynamic-geometry
                :mode :dynamic-mesh)))
