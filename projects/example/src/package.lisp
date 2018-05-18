(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl.core)
  (:local-nicknames (#:v3 #:box.math.vec3)))

(defpackage+ #:fl.example.shaders
  (:use #:cl #:shadow #:box.math.vari))

(defpackage+ #:fl.example.materials
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-material))

(defpackage+ #:fl.example.textures
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-texture))
