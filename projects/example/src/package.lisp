(in-package :defpackage+-user-1)

(defpackage+ #:first-light.example
  (:nicknames #:fl.example)
  (:use #:cl
        #:first-light
        #:first-light.components)
  (:export #:damaged-helmet
           #:geometric-volumes
           #:graph-test
           #:isometric-view-test
           #:noise-test
           #:sprite-test
           #:texture-test))

(defpackage+ #:first-light.example.shaders
  (:nicknames #:fl.example.shaders)
  (:use #:first-light)
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
                #:unlit-texture-cube-map
		#:unlit-texture-cube-map-array))
