(in-package :first-light.example)

(define-scene isometric-view-test ()
  ;; The camera is specified in this specific coordinate hiearchy because we
  ;; need the rotations to compose in a very specific order.
  (@isocam-frame
   ((transform :rotation/incremental (flm:vec3 0 0 (* 0 (/ pi 2)))))
   (@isocam-45
    ((transform :rotation/current (flm:vec3 0
                                            0
                                            (* 1 (- (/ pi 4.0))))))
    (@isocam-35.264
     ((transform :rotation/current (flm:vec3 (* 1 (- (atan (/ (sqrt 2.0)))))
                                             0
                                             0)))
     (@camera
      ;; translate down y axis, rotate to look at origin again.
      ((transform :translation/current (flm:vec3 0 -5 0)
                  :rotation/current (flm:vec3 (* 1 (+ (/ pi 2.0)))
                                              0
                                              0))
       (camera :activep t :mode :orthographic :zoom 100))))))
  (@plane1
   ((transform :translation/current (flm:vec3) ;; -4 3 0
               :rotation/incremental (flm:vec3 (* 0 (/ pi 2))
                                               (* 0 (/ pi 2))
                                               (* 0 (/ pi 2)))
               :scale/current (flm:vec3 1))
    (mesh :location '((:core :mesh) "cube.glb"))
    (mesh-renderer :material 'fl.materials:unlit-texture)))
  (@plane2
   ((transform :translation/current (flm:vec3 2 0 0) ;; -2 3 0
               :rotation/incremental (flm:vec3)
               :scale/current (flm:vec3 1))
    (mesh :location '((:core :mesh) "cube.glb"))
    (mesh-renderer :material 'fl.materials:unlit-texture))))
