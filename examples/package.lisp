(in-package #:cl-user)

(defpackage #:first-light.example
  (:nicknames #:fl.example)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat)
                    )
  (:use #:cl)

  ;; prefab library
  (:export #:examples)

  ;; prefab descriptors
  (:export #:collision-smoke-test
           #:collision-test-0
           #:collision-test-1
           #:damaged-helmet
           #:geometric-volumes
           #:graph
           #:3d-graph-1
           #:3d-graph-2
           #:isometric-view
           #:noise
           #:sprite-1
           #:sprite-2
           #:texture))
