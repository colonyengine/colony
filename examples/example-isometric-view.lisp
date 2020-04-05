(in-package #:virality.examples)

;;; Prefabs

(v:define-prefab "isometric-view" (:library examples)
  ("camera-handle"
   (c/xform:transform :rotate/inc (o:make-velocity v3:+up+
                                                   (float (/ pi 4) 1f0)))
   ("iso"
    (c/xform:transform :rotate (q:orient :local
                                         :x (float (- (atan (/ (sqrt 2)))) 1f0)
                                         :y (float (- (/ pi 4)) 1f0)))
    ("camera"
     (c/xform:transform :translate (v3:vec 0f0 0f0 10f0))
     (c/cam:camera :active-p t
                   :clip-near .1f0
                   :clip-far 1024f0
                   :zoom 100f0
                   :mode :orthographic))))
  ;; TODO: Can't use this, because of transform propogation bug.
  ;; Once that is fixed I can bring this back in.
  #++(("camera" :copy "/cameras/iso")
      ("camera"
       (c/cam:camera (:policy :new-args) :clip-near .1
                                         :clip-far 1024
                                         :zoom 100)))
  ;; NOTE: cubes are on xz plane.
  (("cube-z-1" :copy "/mesh")
   (c/xform:transform :translate (v3:vec 0f0 0f0 -4f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb")))
  (("cube-z-0" :copy "/mesh")
   (c/xform:transform :translate (v3:vec 0f0 0f0 -2f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb")))
  (("cube-origin" :copy "/mesh")
   (c/xform:transform
    #++ :rotate/inc #++ (p:angular-velocity
                         (v3:vec -1f0 1f0 1f0) (float (/ pi 2) 1f0)))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb")))
  (("cube-x-0" :copy "/mesh")
   (c/xform:transform :translate (v3:vec 2f0 0f0 0f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb")))
  (("cube-x-1" :copy "/mesh")
   (c/xform:transform :translate (v3:vec 4f0 0f0 0f0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb"))))

;;; Prefab descriptors
;; '(("isometric-view" examples))
