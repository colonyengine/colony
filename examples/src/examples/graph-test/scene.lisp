(in-package :first-light.example)

(fl:define-scene graph-test (:context context)
  (@camera
   ((fl.comp:transform :translation/current (flm:vec3 0 0 1))
    (fl.comp:camera :active-p t
                    :mode :orthographic)))
  (@graph
   ((fl.comp:transform :scale/current (flm:vec3 (/ (fl:option context :window-width) 2)
                                                (/ (fl:option context :window-height) 2)
                                                0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material 'graph-test))))
