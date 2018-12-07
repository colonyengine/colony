(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl #:fl.comp)
  (:export-only #:damaged-helmet
                #:geometric-volumes
                #:isometric-view-test
                #:sprite-test
                #:texture-test))

(defpackage+ #:fl.example.shaders
  (:use #:fl)
  (:inherit #:shadow.vari)
  (:export-only #:damaged-helmet
                #:noise-2d/sweep-input
                #:sprite-test
                #:unlit-texture-1d
                #:unlit-texture-1d-array
                #:unlit-texture-2d-array
                #:unlit-texture-3d
                #:unlit-texture-cube-map))

(defpackage+ #:fl.example.textures
  (:use #:cl #:fl)
  (:export-only #:texture-test/1d-array-testpat
                #:texture-test/1d-gradient
                #:texture-test/2d-array-testarray
                #:texture-test/2d-wood
                #:texture-test/3d-testpat
                #:texture-test/marble
                #:texture-test/testcubemap))

(defpackage+ #:fl.example.materials
  (:use #:cl #:fl)
  (:export-only #:ssbo/specification-data
                #:texture-test/1d-array-testpat
                #:texture-test/1d-gradient
                #:texture-test/2d-array-testarray
                #:texture-test/2d-sweep-input
                #:texture-test/2d-wood
                #:texture-test/3d-testpat
                #:texture-test/marble
                #:texture-test/testcubemap))
