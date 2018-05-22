(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl.core)
  (:export-only #:damaged-helmet
                #:geometric-volumes))

(defpackage+ #:fl.example.shaders
  (:use #:cl #:shadow #:box.math.vari)
  (:export-only #:damaged-helmet))

(defpackage+ #:fl.example.materials
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-material)
  (:export-only #:damaged-helmet))

(defpackage+ #:fl.example.textures
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-texture)
  (:export-only #:damaged-helmet/metallic-roughness
                #:damaged-helmet/color
                #:damaged-helmet/normal
                #:damaged-helmet/ambient-occlusion
                #:damaged-helmet/emissive))
