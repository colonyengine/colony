(in-package #:defpackage+-user-1)

(defpackage+ #:first-light.gpu.lib
  (:nicknames #:fl.gpu.lib)
  (:inherit-from
   #:fl.gpu
   #:define-function
   #:define-struct
   #:define-macro
   #:define-shader)
  (:inherit
   #:cl
   #:vari)
  ;; structs
  (:export
   #:mesh-attrs
   #:mesh/pos
   #:mesh/normal
   #:mesh/tangent
   #:mesh/color
   #:mesh/uv1
   #:mesh/uv2
   #:mesh/joints
   #:mesh/weights)
  ;; utilities
  (:export
   #:mvlet*)
  ;; math
  (:export
   #:+epsilon+
   #:+pi+
   #:+half-pi+
   #:log10
   #:saturate
   #:map-domain))

(defpackage+ #:first-light.gpu.swizzle
  (:nicknames #:fl.gpu.swizzle)
  (:use #:cl #:vari))

(defpackage+ #:first-light.gpu.user
  (:nicknames #:fl.gpu.user)
  (:use #:fl.gpu.swizzle)
  (:inherit #:fl.gpu.lib))
