(in-package :defpackage+-user-1)

(defpackage+ #:fl.mfiano
  (:use #:cl #:fl.core)
  (:export-only #:sprite-test
                #:graph-test
                #:noise-test))

(defpackage+ #:fl.mfiano.shaders
  (:use #:fl.core #:shadow.lang)
  (:export-only #:screen-draw
                #:sprite-test
                #:graph-test
                #:noise-test))

(defpackage+ #:fl.mfiano.materials
  (:use #:cl #:fl.materials)
  (:import-from #:fl.core #:define-material))

(defpackage+ #:fl.mfiano.textures
  (:use #:cl #:fl.core))
