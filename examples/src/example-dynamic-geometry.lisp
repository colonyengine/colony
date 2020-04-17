(in-package #:virality-examples)

;; Dynamic geometry

(v:define-geometry-layout 2d ()
  (:data (:format interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(v:define-geometry tile ()
  (:layout 2d
   :vertex-count 4
   :primitive :triangle-strip
   :buffers
   (:data (((-0.5 0.5) (0 1))
           ((-0.5 -0.5) (0 0))
           ((0.5 0.5) (1 1))
           ((0.5 -0.5) (1 0))))))

;;; Prefabs

(v:define-material dynamic-geometry
  (:profiles (x:u-mvp)
   :shader ex/shd:dynamic-geometry))

(v:define-prefab "dynamic-geometry" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("plane")
   (comp:transform :rotate/velocity (o:make-velocity v3:+right+ o:pi)
                   :scale (v3:vec 20f0 20f0 20f0))
   (comp:geometry :geometry 'tile)
   (comp:render :material 'dynamic-geometry
                :mode :geometry)))
