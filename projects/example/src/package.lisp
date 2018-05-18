(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl.core)
  (:local-nicknames (#:v3 #:box.math.vec3)))
