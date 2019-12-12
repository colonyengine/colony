(in-package #:virality.examples)

;;; Prefabs

(v:define-prefab "isometric-view" (:library examples)
  ("camera-handle"
   (c/xform:transform :rotate/inc (c/xform:angular-velocity :y (/ pi 4)))
   ("iso"
    (c/xform:transform :rotate (q:orient :local
                                         :x (- (atan (/ (sqrt 2))))
                                         :y (- (/ pi 4))))
    ("camera"
     (c/xform:transform :translate (v3:vec 0 0 10))
     (c/cam:camera :active-p t
                   :clip-near .1
                   :clip-far 1024
                   :zoom 100
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
   (c/xform:transform :translate (v3:vec 0 0 -4))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb")))
  (("cube-z-0" :copy "/mesh")
   (c/xform:transform :translate (v3:vec 0 0 -2))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb")))
  (("cube-origin" :copy "/mesh")
   (c/xform:transform
    #++ :rotate/inc #++ (c/xform:angular-velocity
                         (v3:vec -1f0 1f0 1f0) (/ pi 2)))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb")))
  (("cube-x-0" :copy "/mesh")
   (c/xform:transform :translate (v3:vec 2 0 0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb")))
  (("cube-x-1" :copy "/mesh")
   (c/xform:transform :translate (v3:vec 4 0 0))
   (c/smesh:static-mesh :asset '(:virality.engine/mesh "cube.glb"))))

;;; Prefab descriptors

(v:define-prefab-descriptor isometric-view ()
  ("isometric-view" examples))
