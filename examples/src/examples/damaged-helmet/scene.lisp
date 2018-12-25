(in-package :first-light.example)

(fl:define-scene damaged-helmet ()
  (@camera
   ((fl.comp:transform :translate (flm:vec3 0 0 50))
    (fl.comp:camera :active-p t
                    :mode :perspective
                    :zoom 10)))
  (@damaged-helmet
   ((fl.comp:transform :rotate (flm:vec3 (/ pi 2) 0 0)
                       :rotate/inc (flm:vec3 0 0.6 0)
                       :scale (flm:vec3 4))
    (fl.comp:mesh :location '(:mesh "damaged-helmet.glb"))
    (fl.comp:mesh-renderer :material 'damaged-helmet))))
