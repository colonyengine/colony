(in-package :defpackage+-user-1)

(defpackage+ #:first-light.example
  (:nicknames #:fl.example)
  (:local-nicknames (#:m #:game-math))
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
