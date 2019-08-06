(in-package #:virality.examples)

;;; Materials

(v:define-material noise
  (:profiles (x/mat:u-mvpt)
   :shader ex/shd:noise))

;;; Prefabs

(v:define-prefab "noise-tile" (:library examples)
  (c/xform:transform :scale (v3:vec 90 90 0))
  (c/smesh:static-mesh :location '((:core :mesh) "plane.glb")))

(v:define-prefab "noise" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("perlin-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -540 202.5 0))
   (c/render:render :material '(noise
                                noise/perlin-3d
                                :shader ex/shd:noise/perlin-3d)))
  (("perlin-surflet-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -325 202.5 0))
   (c/render:render
    :material '(noise
                noise/perlin-surflet-3d
                :shader ex/shd:noise/perlin-surflet-3d)))
  (("perlin-improved-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -110 202.5 0))
   (c/render:render
    :material '(noise
                noise/perlin-improved-3d
                :shader ex/shd:noise/perlin-improved-3d)))
  (("perlin-4d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec 110 202.5 0))
   (c/render:render :material '(noise
                                noise/perlin-4d
                                :shader ex/shd:noise/perlin-4d)))
  (("cellular-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec 325 202.5 0))
   (c/render:render :material '(noise
                                noise/cellular-3d
                                :shader ex/shd:noise/cellular-3d)))
  (("cellular-fast-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec 540 202.5 0))
   (c/render:render
    :material '(noise
                noise/cellular-fast-3d
                :shader ex/shd:noise/cellular-fast-3d)))
  (("hermite-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -540 -22.5 0))
   (c/render:render :material '(noise
                                noise/hermite-3d
                                :shader ex/shd:noise/hermite-3d)))
  (("simplex-perlin-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -325 -22.5 0))
   (c/render:render
    :material '(noise
                noise/simplex-perlin-3d
                :shader ex/shd:noise/simplex-perlin-3d)))
  (("simplex-cellular-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -110 -22.5 0))
   (c/render:render
    :material '(noise
                noise/simplex-cellular-3d
                :shader ex/shd:noise/simplex-cellular-3d)))
  (("simplex-polkadot-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec 110 -22.5 0))
   (c/render:render
    :material '(noise
                noise/simplex-polkadot-3d
                :shader ex/shd:noise/simplex-polkadot-3d)))
  (("value-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec 325 -22.5 0))
   (c/render:render :material '(noise
                                noise/value-3d
                                :shader ex/shd:noise/value-3d)))
  (("value-4d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec 540 -22.5 0))
   (c/render:render :material '(noise
                                noise/value-4d
                                :shader ex/shd:noise/value-4d)))
  (("value-hermite-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -540 -247.5 0))
   (c/render:render
    :material '(noise
                noise/value-hermite-3d
                :shader ex/shd:noise/value-hermite-3d)))
  (("value-perlin-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -325 -247.5 0))
   (c/render:render :material '(noise
                                noise/value-perlin-3d
                                :shader ex/shd:noise/value-perlin-3d)))
  (("polkadot-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec -110 -247.5 0))
   (c/render:render :material '(noise
                                noise/polkadot-3d
                                :shader ex/shd:noise/polkadot-3d)))
  (("polkadot-box-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec 110 -247.5 0))
   (c/render:render :material '(noise
                                noise/polkadot-box-3d
                                :shader ex/shd:noise/polkadot-box-3d)))
  (("cubist-3d" :copy "/noise-tile")
   (c/xform:transform (:policy :new-args) :translate (v3:vec 325 -247.5 0))
   (c/render:render :material '(noise
                                noise/cubist-3d
                                :shader ex/shd:noise/cubist-3d))))

;;; Prefab descriptors

(v:define-prefab-descriptor noise ()
  ("noise" examples))
