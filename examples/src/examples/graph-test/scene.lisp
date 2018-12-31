(in-package :first-light.example)

(fl:define-scene graph-test (:context context)
  (@camera
   ((fl.comp:transform :translate (flm:vec3 0 0.01 1))
    (fl.comp:camera :active-p t
                    :mode :perspective)))
  (@graph
   ((fl.comp:transform :scale (flm:vec3 (/ (fl:option context :window-width) 2)
                                        (/ (fl:option context :window-height) 2)
                                        0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material 'graph-test))))

(fl:define-scene 3d-graph-test/1 (:context context)
  (@camera
   ((fl.comp:transform :translate (flm:vec3 0 70 100))
    (fl.comp:camera :active-p t
                    :mode :perspective
                    :zoom 2)
    (fl.comp:tracking-camera :target-actor @graph)))
  (@graph
   ((fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(3d-graph-test
                                       3d-graph-test/1
                                       :shader fl.gpu.user:3d-graph-test/1
                                       :instances 100000
                                       :uniforms ((:size 0.5)))))))


(fl:define-scene 3d-graph-test/2 (:context context)
  (@camera
   ((fl.comp:transform :translate (flm:vec3 0 50 100))
    (fl.comp:camera :active-p t
                    :mode :perspective
                    :zoom 2)
    (fl.comp:tracking-camera :target-actor @graph)))
  (@graph
   ((fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:mesh-renderer :material '(3d-graph-test
                                       3d-graph-test/2
                                       :shader fl.gpu.user:3d-graph-test/2
                                       :instances 100000
                                       :uniforms ((:size 1)))))))
