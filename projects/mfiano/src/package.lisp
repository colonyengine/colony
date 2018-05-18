(in-package :defpackage+-user-1)

(defpackage+ #:fl.mfiano
  (:use #:cl #:fl.core)
  (:local-nicknames (#:v3 #:box.math.vec3)))

(defpackage+ #:fl.mfiano.shaders
  (:use #:cl #:shadow #:box.math.vari)
  (:export-only #:sprite-shader))

(defpackage+ #:fl.mfiano.materials
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-material)
  (:export-only #:sprite))

(defpackage+ #:fl.mfiano.textures
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-texture)
  (:export-only #:sprites))
