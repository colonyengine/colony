(in-package :defpackage+-user-1)

(defpackage+ #:fl.materials
  (:use #:cl #:shadow)
  (:inherit #:box.math.vari #:vari)
  (:export
   ;; Materials defined in FL core.
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal))
