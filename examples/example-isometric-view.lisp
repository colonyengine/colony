(in-package #:first-light.example)

;;; Prefabs

(fl:define-prefab "isometric-view" (:library examples)
  ("camera-handle"
   (fl.comp:transform :rotate/inc (q:orient :local :y (/ pi 4)))
   ("iso"
    (fl.comp:transform :rotate (q:orient :local
                                         :x (- (atan (/ (sqrt 2))))
                                         :y (- (/ pi 4))))
    ("camera"
     (fl.comp:transform :translate (v3:make 0 0 10))
     (fl.comp:camera :active-p t
                     :clip-near .1
                     :clip-far 1024
                     :zoom 100
                     :mode :orthographic))))

  ;; TODO: Can't use this, because of transform propogation bug.
  ;; Once that is fixed I can bring this back in.
  #++(("camera" :copy "/cameras/iso")
      ("camera"
       (fl.comp:camera (:policy :new-args) :clip-near .1
                                           :clip-far 1024
                                           :zoom 100)))

  ;; NOTE: cubes are on xz plane.

  (("cube-z-1" :copy "/mesh")
   (fl.comp:transform :translate (v3:make 0 0 -4))
   (fl.comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-z-0" :copy "/mesh")
   (fl.comp:transform :translate (v3:make 0 0 -2))
   (fl.comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-origin" :copy "/mesh")
   (fl.comp:transform #++ :rotate/inc #++ (q:orient :local
                                                    (v3:make -1f0 1f0 1f0) (/ pi 2)))
   (fl.comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-x-0" :copy "/mesh")
   (fl.comp:transform :translate (v3:make 2 0 0))
   (fl.comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-x-1" :copy "/mesh")
   (fl.comp:transform :translate (v3:make 4 0 0))
   (fl.comp:static-mesh :location '((:core :mesh) "cube.glb"))))


;;; Prefab descriptors

(fl:define-prefab-descriptor isometric-view ()
  ("isometric-view" fl.example:examples))
