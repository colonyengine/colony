(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl)
  (:export-only #:damaged-helmet
                #:geometric-volumes))

(defpackage+ #:fl.example.shaders
  (:use #:fl #:shadow.lang)
  (:export-only #:damaged-helmet))

(defpackage+ #:fl.example.materials
  (:use #:cl #:fl))

(defpackage+ #:fl.example.textures
  (:use #:cl #:fl))
