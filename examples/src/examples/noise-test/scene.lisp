(in-package :first-light.example)

(fl:define-scene noise-test ()
  (@camera
   ((fl.comp:transform :translation/current (flm:vec3 0 0 1))
    (fl.comp:camera :active-p t
                    :mode :orthographic)))
  (@perlin-3d
   ((fl.comp:transform :translation/current (flm:vec3 -540 202.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/perlin-3d
                                       :shader fl.shader:noise-test/perlin-3d))))
  (@perlin-surflet-3d
   ((fl.comp:transform :translation/current (flm:vec3 -325 202.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/perlin-surflet-3d
                                       :shader fl.shader:noise-test/perlin-surflet-3d))))
  (@perlin-improved-3d
   ((fl.comp:transform :translation/current (flm:vec3 -110 202.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/perlin-improved-3d
                                       :shader fl.shader:noise-test/perlin-improved-3d))))
  (@perlin-4d
   ((fl.comp:transform :translation/current (flm:vec3 110 202.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/perlin-4d
                                       :shader fl.shader:noise-test/perlin-4d))))
  (@cellular-3d
   ((fl.comp:transform :translation/current (flm:vec3 325 202.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/cellular-3d
                                       :shader fl.shader:noise-test/cellular-3d))))
  (@cellular-fast-3d
   ((fl.comp:transform :translation/current (flm:vec3 540 202.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/cellular-fast-3d
                                       :shader fl.shader:noise-test/cellular-fast-3d))))
  (@hermite-3d
   ((fl.comp:transform :translation/current (flm:vec3 -540 -22.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/hermite-3d
                                       :shader fl.shader:noise-test/hermite-3d))))
  (@simplex-perlin-3d
   ((fl.comp:transform :translation/current (flm:vec3 -325 -22.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/simplex-perlin-3d
                                       :shader fl.shader:noise-test/simplex-perlin-3d))))
  (@simplex-cellular-3d
   ((fl.comp:transform :translation/current (flm:vec3 -110 -22.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/simplex-cellular-3d
                                       :shader fl.shader:noise-test/simplex-cellular-3d))))
  (@simplex-polkadot-3d
   ((fl.comp:transform :translation/current (flm:vec3 110 -22.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/simplex-polkadot-3d
                                       :shader fl.shader:noise-test/simplex-polkadot-3d))))
  (@value-3d
   ((fl.comp:transform :translation/current (flm:vec3 325 -22.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/value-3d
                                       :shader fl.shader:noise-test/value-3d))))
  (@value-4d
   ((fl.comp:transform :translation/current (flm:vec3 540 -22.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/value-4d
                                       :shader fl.shader:noise-test/value-4d))))
  (@value-hermite-3d
   ((fl.comp:transform :translation/current (flm:vec3 -540 -247.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/value-hermite-3d
                                       :shader fl.shader:noise-test/value-hermite-3d))))
  (@value-perlin-3d
   ((fl.comp:transform :translation/current (flm:vec3 -325 -247.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/value-perlin-3d
                                       :shader fl.shader:noise-test/value-perlin-3d))))
  (@polkadot-3d
   ((fl.comp:transform :translation/current (flm:vec3 -110 -247.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/polkadot-3d
                                       :shader fl.shader:noise-test/polkadot-3d))))
  (@polkadot-box-3d
   ((fl.comp:transform :translation/current (flm:vec3 110 -247.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/polkadot-box-3d
                                       :shader fl.shader:noise-test/polkadot-box-3d))))
  (@cubist-3d
   ((fl.comp:transform :translation/current (flm:vec3 325 -247.5 0)
                       :scale/current (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(noise-test
                                       noise-test/cubist-3d
                                       :shader fl.shader:noise-test/cubist-3d)))))
