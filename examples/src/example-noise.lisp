(in-package #:virality-examples)

;;; Materials

(v:define-material noise
  (:profiles (x:u-mvpt)
   :shader ex/shd:noise))

;;; Prefabs

(v:define-prefab "noise-tile" (:library examples)
  (comp:transform :scale (v3:vec 90f0 90f0 0f0))
  (comp:mesh :asset '(v::meshes v::primitives)
             :name "plane"))

(v:define-prefab "noise" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("perlin-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -540f0 202.5f0 0f0))
   (comp:render :material '(noise
                            noise/perlin-3d
                            :shader ex/shd:noise/perlin-3d)))
  (("perlin-surflet-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -325f0 202.5f0 0f0))
   (comp:render
    :material '(noise
                noise/perlin-surflet-3d
                :shader ex/shd:noise/perlin-surflet-3d)))
  (("perlin-improved-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -110f0 202.5f0 0f0))
   (comp:render
    :material '(noise
                noise/perlin-improved-3d
                :shader ex/shd:noise/perlin-improved-3d)))
  (("perlin-4d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 110f0 202.5f0 0f0))
   (comp:render :material '(noise
                            noise/perlin-4d
                            :shader ex/shd:noise/perlin-4d)))
  (("cellular-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 325f0 202.5f0 0f0))
   (comp:render :material '(noise
                            noise/cellular-3d
                            :shader ex/shd:noise/cellular-3d)))
  (("cellular-fast-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 540f0 202.5f0 0f0))
   (comp:render
    :material '(noise
                noise/cellular-fast-3d
                :shader ex/shd:noise/cellular-fast-3d)))
  (("hermite-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -540f0 -22.5f0 0f0))
   (comp:render :material '(noise
                            noise/hermite-3d
                            :shader ex/shd:noise/hermite-3d)))
  (("simplex-perlin-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -325f0 -22.5f0 0f0))
   (comp:render
    :material '(noise
                noise/simplex-perlin-3d
                :shader ex/shd:noise/simplex-perlin-3d)))
  (("simplex-cellular-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -110f0 -22.5f0 0f0))
   (comp:render
    :material '(noise
                noise/simplex-cellular-3d
                :shader ex/shd:noise/simplex-cellular-3d)))
  (("simplex-polkadot-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 110f0 -22.5f0 0f0))
   (comp:render
    :material '(noise
                noise/simplex-polkadot-3d
                :shader ex/shd:noise/simplex-polkadot-3d)))
  (("value-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 325f0 -22.5f0 0f0))
   (comp:render :material '(noise
                            noise/value-3d
                            :shader ex/shd:noise/value-3d)))
  (("value-4d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 540f0 -22.5f0 0f0))
   (comp:render :material '(noise
                            noise/value-4d
                            :shader ex/shd:noise/value-4d)))
  (("value-hermite-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -540f0 -247.5f0 0f0))
   (comp:render
    :material '(noise
                noise/value-hermite-3d
                :shader ex/shd:noise/value-hermite-3d)))
  (("value-perlin-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -325f0 -247.5f0 0f0))
   (comp:render :material '(noise
                            noise/value-perlin-3d
                            :shader ex/shd:noise/value-perlin-3d)))
  (("polkadot-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec -110f0 -247.5f0 0f0))
   (comp:render :material '(noise
                            noise/polkadot-3d
                            :shader ex/shd:noise/polkadot-3d)))
  (("polkadot-box-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 110f0 -247.5f0 0f0))
   (comp:render :material '(noise
                            noise/polkadot-box-3d
                            :shader ex/shd:noise/polkadot-box-3d)))
  (("cubist-3d" :copy "/noise-tile")
   (comp:transform (:policy :new-args)
                   :translate (v3:vec 325f0 -247.5f0 0f0))
   (comp:render :material '(noise
                            noise/cubist-3d
                            :shader ex/shd:noise/cubist-3d))))
