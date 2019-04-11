(in-package :defpackage+-user-1)

(defpackage+ #:first-light.gpu.visualization
  (:nicknames #:fl.gpu.visualization)
  (:use #:fl.gpu.lib
        #:fl.gpu.swizzle)
  (:export
   #:collider/sphere))
