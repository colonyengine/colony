(in-package :first-light.example)

(fl:define-scene geometric-volumes ()
  (@camera
   ((fl.comp:transform :translate (flm:vec3 0 0 50))
    (fl.comp:camera :active-p t
                    :mode :perspective)))
  (@plane
   ((fl.comp:transform :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@cube
   ((fl.comp:transform :translate (flm:vec3 0 30 0)
                       :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@sphere
   ((fl.comp:transform :translate (flm:vec3 0 -30 0)
                       :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "sphere.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@torus
   ((fl.comp:transform :translate (flm:vec3 30 0 0)
                       :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "torus.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@cone
   ((fl.comp:transform :translate (flm:vec3 -30 0 0)
                       :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "cone.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture-decal-bright))))
