(in-package :defpackage+-user-1)

(defpackage+ #:fl.shaders
  (:use #:cl #:shadow)
  (:inherit #:box.math.vari #:vari)
  (:export
   ;; shader programs defined in *.shader-programs files that are in this
   ;; package.
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal
   #:pbr-damaged-helmet
   ))
