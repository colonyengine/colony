(in-package :defpackage+-user-1)

(defpackage+ #:fl.materials
  (:use #:cl #:shadow)
  (:local-nicknames
   (#:v2 #:box.math.vec2)
   (#:v3 #:box.math.vec3)
   (#:v4 #:box.math.vec4)
   (#:m2 #:box.math.mat2)
   (#:m3 #:box.math.mat3)
   (#:m4 #:box.math.mat4)
   (#:q #:box.math.quat))
  (:import-from #:fl.core
                #:define-material)
  (:export
   ;; Materials defined in FL core.
   #:missing-material
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal
   #:pbr-damaged-helmet
   ))
