(in-package #:virality.examples)

;;; Prefabs

(v:define-prefab "isometric-view" (:library examples)
  ("camera-handle"
   (comp:transform :rotate/inc (q:orient :local :y (/ pi 4)))
   ("iso"
    (comp:transform :rotate (q:orient :local
                                      :x (- (atan (/ (sqrt 2))))
                                      :y (- (/ pi 4))))
    ("camera"
     (comp:transform :translate (v3:vec 0 0 10))
     (comp:camera :active-p t
                  :clip-near .1
                  :clip-far 1024
                  :zoom 100
                  :mode :orthographic))))

  ;; TODO: Can't use this, because of transform propogation bug.
  ;; Once that is fixed I can bring this back in.
  #++(("camera" :copy "/cameras/iso")
      ("camera"
       (comp:camera (:policy :new-args) :clip-near .1
                                        :clip-far 1024
                                        :zoom 100)))

  ;; NOTE: cubes are on xz plane.

  (("cube-z-1" :copy "/mesh")
   (comp:transform :translate (v3:vec 0 0 -4))
   (comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-z-0" :copy "/mesh")
   (comp:transform :translate (v3:vec 0 0 -2))
   (comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-origin" :copy "/mesh")
   (comp:transform #++ :rotate/inc #++ (q:orient :local
                                                 (v3:vec -1f0 1f0 1f0) (/ pi 2)))
   (comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-x-0" :copy "/mesh")
   (comp:transform :translate (v3:vec 2 0 0))
   (comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-x-1" :copy "/mesh")
   (comp:transform :translate (v3:vec 4 0 0))
   (comp:static-mesh :location '((:core :mesh) "cube.glb"))))


;;; Prefab descriptors

(v:define-prefab-descriptor isometric-view ()
  ("isometric-view" examples))
