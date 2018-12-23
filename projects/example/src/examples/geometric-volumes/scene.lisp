(in-package :first-light.example)

(fl:define-scene geometric-volumes ()
  (@camera
   ((fl.comp:transform :translation/current (flm:vec3 0 0 50))
    (fl.comp:camera :activep t :mode :perspective)))
  (@plane
   ((fl.comp:transform :rotation/incremental (flm:vec3 1)
                       :scale/current (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@cube
   ((fl.comp:transform :translation/current (flm:vec3 0 30 0)
                       :rotation/incremental (flm:vec3 1)
                       :scale/current (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@sphere
   ((fl.comp:transform :translation/current (flm:vec3 0 -30 0)
                       :rotation/incremental (flm:vec3 1)
                       :scale/current (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "sphere.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@torus
   ((fl.comp:transform :translation/current (flm:vec3 30 0 0)
                       :rotation/incremental (flm:vec3 1)
                       :scale/current (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "torus.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@cone
   ((fl.comp:transform :translation/current (flm:vec3 -30 0 0)
                       :rotation/incremental (flm:vec3 1)
                       :scale/current (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "cone.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture-decal-bright))))
