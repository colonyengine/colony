(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl.core)
  (:export-only #:damaged-helmet
                #:geometric-volumes))

(defpackage+ #:fl.example.shaders
  (:use #:fl.core #:shadow.lang)
  (:export-only #:damaged-helmet))

(defpackage+ #:fl.example.materials
  (:use #:cl #:fl.core))

(defpackage+ #:fl.example.textures
  (:use #:cl #:fl.core))
