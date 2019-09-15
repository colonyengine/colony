(in-package #:virality.examples)

;;; Textures

(v:define-texture art5/texture (:texture-2d)
  (:data #((:playground-tex "city.tiff"))))

;;; Materials

(v:define-material art1
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd:art1))

(v:define-material art2
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd:art2))

(v:define-material art3
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd:art3))

(v:define-material art4
  (:profiles (x/mat:u-mvptr)
   :uniforms ((:zoom 0.85)
              (:speed 1)
              (:strength 0.7)
              (:colorize nil)
              (:outline nil)
              (:detail 0.8))
   :shader ex/shd:art4))

(v:define-material art5
  (:profiles (x/mat:u-mvptr)
   :uniforms ((:blur2 7.5)
              (:sampler 'art5/texture))
   :shader ex/shd:art5))

;;; Prefabs

(v:define-prefab "art1" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                     (/ (v:option context :window-height) 2)
                                     0))
   (c/render:render :material 'art1)))

(v:define-prefab "art2" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                     (/ (v:option context :window-height) 2)
                                     0))
   (c/render:render :material 'art2)))

(v:define-prefab "art3" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                     (/ (v:option context :window-height) 2)
                                     0))
   (c/render:render :material 'art3)))

(v:define-prefab "art4" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                     (/ (v:option context :window-height) 2)
                                     0))
   (c/render:render :material 'art4)))

(v:define-prefab "art5" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                     (/ (v:option context :window-height) 2)
                                     0))
   (c/render:render :material 'art5)))

;;; Prefab descriptors

(v:define-prefab-descriptor art1 ()
  ("art1" examples))

(v:define-prefab-descriptor art2 ()
  ("art2" examples))

(v:define-prefab-descriptor art3 ()
  ("art3" examples))

(v:define-prefab-descriptor art4 ()
  ("art4" examples))

(v:define-prefab-descriptor art5 ()
  ("art5" examples))
