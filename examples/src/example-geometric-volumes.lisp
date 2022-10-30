(in-package #:virality-examples)

;;; Prefabs

(v:define-prefab "geometric-volumes" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera :zoom 3))
  (("plane" :copy "/mesh")
   (comp:transform :rotate/velocity (v3:velocity (v3:ones) o:pi)))
  (("cube" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 5f0 0f0)
                   :rotate/velocity (v3:velocity (v3:ones) o:pi))
   (comp:mesh :name "cube"))
  (("sphere" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 -5f0 0f0)
                   :rotate/velocity (v3:velocity (v3:ones) o:pi))
   (comp:mesh :name "sphere"))
  (("torus" :copy "/mesh")
   (comp:transform :translate (v3:vec 5f0 0f0 0f0)
                   :rotate/velocity (v3:velocity (v3:ones) o:pi))
   (comp:mesh :name "torus"))
  (("cone" :copy "/mesh")
   (comp:transform :translate (v3:vec -5f0 0f0 0f0)
                   :rotate/velocity (v3:velocity (v3:ones) o:pi))
   (comp:mesh :name "cone")
   (comp:render :material 'x:unlit-texture-decal-bright
                :slave (v:ref :self :component 'comp:mesh))))
