(in-package :defpackage+-user-1)

(defpackage+ #:fl.mfiano
  (:use #:cl #:fl)
  (:export-only #:sprite-test
                #:graph-test
                #:noise-test
                #:noise-test/playground
                #:mesh-test))

(defpackage+ #:fl.mfiano.shaders
  (:use #:fl #:shadow.lang)
  (:export-only #:screen-draw
                #:sprite-test
                #:graph-test
                #:noise-test
                #:noise-test/playground
                #:mesh-test))

(defpackage+ #:fl.mfiano.materials
  (:use #:cl #:fl.materials)
  (:import-from #:fl #:define-material))

(defpackage+ #:fl.mfiano.textures
  (:use #:cl #:fl))
