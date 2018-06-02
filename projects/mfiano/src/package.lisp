(in-package :defpackage+-user-1)

(defpackage+ #:fl.mfiano
  (:use #:cl #:fl.core)
  (:export-only #:2d-sprites
                #:noise-test))

(defpackage+ #:fl.mfiano.shaders
  (:use #:fl.core #:shadow.lang)
  (:export-only #:sprite-shader
                #:screen-draw
                #:noise-test))

(defpackage+ #:fl.mfiano.materials
  (:use #:cl #:fl.core))

(defpackage+ #:fl.mfiano.textures
  (:use #:cl #:fl.core))
