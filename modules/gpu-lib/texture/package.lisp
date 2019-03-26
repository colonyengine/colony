(in-package :defpackage+-user-1)

(defpackage+ #:first-light.gpu.texture
  (:nicknames #:fl.gpu.texture)
  (:use #:fl.gpu.lib
        #:fl.gpu.swizzle)
  (:export
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal))
