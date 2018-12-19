(in-package :first-light.example)

(define-scene damaged-helmet ()
  (@camera
   ((transform :translation/current (flm:vec3 0 0 50))
    (camera :activep t :mode :perspective :zoom 10)))
  (@damaged-helmet
   ((transform :rotation/current (flm:vec3 (/ pi 2) 0 0)
               :rotation/incremental (flm:vec3 0 0.6 0)
               :scale/current (flm:vec3 4))
    (mesh :location '(:mesh "damaged-helmet.glb"))
    (mesh-renderer :material 'damaged-helmet))))
