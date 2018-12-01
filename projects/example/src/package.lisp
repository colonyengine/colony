(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl)
  (:export-only #:damaged-helmet
                #:geometric-volumes
                #:texture-test
                #:isometric-view-test))

(defpackage+ #:fl.example.shaders
  (:use #:fl)
  (:inherit #:shadow.vari)
  (:export-only #:damaged-helmet
                #:unlit-texture-1d
                #:unlit-texture-3d
                #:unlit-texture-1d-array))

(defpackage+ #:fl.example.materials
  (:use #:cl #:fl)
  (:export-only #:texture-test/1d-gradient
                #:texture-test/2d-wood
                #:texture-test/3d-testpat
                #:texture-test/marble
                #:texture-test/1d-array-testpat))

(defpackage+ #:fl.example.textures
  (:use #:cl #:fl)
  (:export-only #:texture-test/1d-gradient
                #:texture-test/2d-wood
                #:texture-test/3d-testpat
                #:texture-test/marble
                #:texture-test/1d-array-testpat))
