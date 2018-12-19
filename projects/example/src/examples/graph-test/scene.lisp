(in-package :first-light.example)

(define-scene graph-test ()
  (@camera
   ((transform :translation/current (flm:vec3 0 0 1))
    (camera :activep t :mode :orthographic)))
  (@graph
   ((transform :scale/current (flm:vec3 (/ (option context :window-width) 2)
                                        (/ (option context :window-height) 2)
                                        0))
    (mesh :location '((:core :mesh) "plane.glb"))
    (mesh-renderer :material 'graph-test))))
