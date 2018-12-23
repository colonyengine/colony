(in-package :first-light.example)

(fl:define-scene isometric-view-test ()
  ;; The camera is specified in this specific coordinate hiearchy because we
  ;; need the rotations to compose in a very specific order.
  (@isocam-frame
   ((fl.comp:transform :rotation/incremental (flm:vec3)))
   (@isocam-45
    ((fl.comp:transform :rotation/current (flm:vec3 0 0 (- (/ pi 4.0)))))
    (@isocam-35.264
     ((fl.comp:transform :rotation/current (flm:vec3 (- (atan (/ (sqrt 2.0)))) 0 0)))
     (@camera
      ;; translate down y axis, rotate to look at origin again.
      ((fl.comp:transform :translation/current (flm:vec3 0 -5 0)
                          :rotation/current (flm:vec3 (+ (/ pi 2.0)) 0 0))
       (fl.comp:camera :activep t :mode :orthographic :zoom 100))))))
  (@plane1
   ((fl.comp:transform :translation/current (flm:vec3)
                       :rotation/incremental (flm:vec3 (/ pi 2) (/ pi 2) (/ pi 2))
                       :scale/current (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture)))
  (@plane2
   ((fl.comp:transform :translation/current (flm:vec3 2 0 0)
                       :rotation/incremental (flm:vec3)
                       :scale/current (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:mesh-renderer :material 'fl.materials:unlit-texture))))
