(in-package :defpackage+-user-1)

(defpackage+ #:fl.shaders
  (:use #:%fl)
  (:inherit #:shadow.vari)
  (:export-only #:unlit-color
                #:unlit-color-decal
                #:unlit-texture
                #:unlit-texture-decal))
