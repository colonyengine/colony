(in-package #:virality.examples)

;;; Materials

(v:define-material art1
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd::art1))

(v:define-material art2
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd::art2))

(v:define-material art3
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd::art3))

;;; Prefabs

(v:define-prefab "art1" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("graph" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                     (/ (v:option context :window-height) 2)
                                     0))
   (c/render:render :material 'art1)))

(v:define-prefab "art2" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("graph" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                     (/ (v:option context :window-height) 2)
                                     0))
   (c/render:render :material 'art2)))

(v:define-prefab "art3" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("graph" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                     (/ (v:option context :window-height) 2)
                                     0))
   (c/render:render :material 'art3)))

;;; Prefab descriptors

(v:define-prefab-descriptor art1 ()
  ("art1" examples))

(v:define-prefab-descriptor art2 ()
  ("art2" examples))

(v:define-prefab-descriptor art3 ()
  ("art3" examples))
