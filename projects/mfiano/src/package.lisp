(in-package :defpackage+-user-1)

(defpackage+ #:fl.mfiano
  (:use #:cl #:fl.core)
  (:export-only #:2d-sprites
                #:noise-test))

(defpackage+ #:fl.mfiano.shaders
  (:use #:shadow #:shadow.lang)
  (:export-only #:sprite-shader
                #:screen-draw
                #:noise-test))

(defpackage+ #:fl.mfiano.materials
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-material)
  (:export-only #:sprite
                #:screen-draw-test
                #:noise-test))

(defpackage+ #:fl.mfiano.textures
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-texture)
  (:export-only #:sprites))
