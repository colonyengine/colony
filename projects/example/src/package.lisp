(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl #:fl.comp)
  (:export #:damaged-helmet
           #:geometric-volumes
           #:graph-test
           #:isometric-view-test
           #:sprite-test
           #:texture-test))

(defpackage+ #:fl.example.shaders
  (:use #:fl)
  (:inherit #:shadow.vari)
  (:export-only #:damaged-helmet
                #:graph-test
                #:noise-2d/sweep-input
                #:noise-test
                #:sprite-test
                #:unlit-texture-1d
                #:unlit-texture-1d-array
                #:unlit-texture-2d-array
                #:unlit-texture-3d
                #:unlit-texture-cube-map))
