(in-package :defpackage+-user-1)

(defpackage+ #:fl.example.mfiano
  (:use #:cl #:fl.core)
  (:local-nicknames (#:v3 #:box.math.vec3)))

(defpackage+ #:fl.example.mfiano.shaders
  (:use #:cl #:shadow #:box.math.vari)
  (:export-only #:sprite-shader))

(defpackage+ #:fl.example.mfiano.materials
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-material)
  (:export-only #:sprite))

(defpackage+ #:fl.example.mfiano.textures
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-texture)
  (:export-only #:sprites))
