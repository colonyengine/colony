(in-package :first-light.example)

(define-scene geometric-volumes ()
  (@camera
   ((transform :translation/current (flm:vec3 0 0 50))
    (camera :activep t :mode :perspective)))
  (@plane
   ((transform :rotation/incremental (flm:vec3 1)
               :scale/current (flm:vec3 6))
    (mesh :location '((:core :mesh) "plane.glb"))
    (mesh-renderer :material 'fl.materials:unlit-texture)))
  (@cube
   ((transform :translation/current (flm:vec3 0 30 0)
               :rotation/incremental (flm:vec3 1)
               :scale/current (flm:vec3 6))
    (mesh :location '((:core :mesh) "cube.glb"))
    (mesh-renderer :material 'fl.materials:unlit-texture)))
  (@sphere
   ((transform :translation/current (flm:vec3 0 -30 0)
               :rotation/incremental (flm:vec3 1)
               :scale/current (flm:vec3 6))
    (mesh :location '((:core :mesh) "sphere.glb"))
    (mesh-renderer :material 'fl.materials:unlit-texture)))
  (@torus
   ((transform :translation/current (flm:vec3 30 0 0)
               :rotation/incremental (flm:vec3 1)
               :scale/current (flm:vec3 6))
    (mesh :location '((:core :mesh) "torus.glb"))
    (mesh-renderer :material 'fl.materials:unlit-texture)))
  (@cone
   ((transform :translation/current (flm:vec3 -30 0 0)
               :rotation/incremental (flm:vec3 1)
               :scale/current (flm:vec3 6))
    (mesh :location '((:core :mesh) "cone.glb"))
    (mesh-renderer :material 'fl.materials:unlit-texture-decal-bright))))
