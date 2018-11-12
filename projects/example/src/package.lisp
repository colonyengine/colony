(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl)
  (:export-only #:damaged-helmet
                #:geometric-volumes
                #:texture-test
                #:broken-scene))

(defpackage+ #:fl.example.shaders
  (:use #:fl #:shadow.lang)
  (:export-only #:damaged-helmet
                #:unlit-color-1d))

(defpackage+ #:fl.example.materials
  (:use #:cl #:fl)
  (:export-only #:texture-test/1d-gradient
                #:texture-test/2d-wood))

(defpackage+ #:fl.example.textures
  (:use #:cl #:fl)
  (:export-only #:texture-test/1d-gradient
                #:texture-test/2d-wood))
