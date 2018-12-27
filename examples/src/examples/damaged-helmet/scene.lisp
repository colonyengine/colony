(in-package :first-light.example)

(fl:define-scene damaged-helmet ()
  (@camera
   ((fl.comp:transform :translation/current (flm:vec3 0 0 50))
    (fl.comp:camera :active-p t
                    :mode :perspective
                    :zoom 10)))
  (@damaged-helmet
   ((fl.comp:transform :rotation/current (flm:vec3 (/ pi 2) 0 0)
                       :rotation/incremental (flm:vec3 0 0 -0.6)
                       :scale/current (flm:vec3 4))
    (fl.comp:mesh :location '(:mesh "damaged-helmet.glb"))
    (fl.comp:mesh-renderer :material 'damaged-helmet))))
