(in-package :defpackage+-user-1)

(defpackage+ #:fl.psilord
  (:use #:cl #:fl #:fl.comp)
  (:export-only #:demo
                #:material-test))

(defpackage+ #:fl.psilord.shaders
  (:use #:fl)
  (:inherit #:shadow.vari)
  (:export-only #:test-shader-0))

(defpackage+ #:fl.psilord.materials
  (:use #:cl #:fl))

(defpackage+ #:fl.psilord.textures
  (:use #:cl #:fl))
