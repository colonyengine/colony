(in-package :defpackage+-user-1)

(defpackage+ #:first-light.shader
  (:nicknames #:fl.shader)
  (:use #:cl
        #:vari)
  (:shadow #:cl
           #:defun
           #:defstruct
           #:defmacro)
  (:import-from #:varjo
                #:v-def-glsl-template-fun
                #:v-float
                #:v-vec2
                #:v-vec3
                #:v-vec4
                #:v-mat4)
  (:export #:defun
           #:defstruct
           #:defmacro
           #:define-shader
           #:load-shaders
           #:unload-shaders
           #:recompile-shaders
           #:with-shader
           #:view-source
           #:create-block-alias
           #:find-block
           #:bind-block
           #:unbind-block
           #:buffer-name
           #:find-buffer
           #:create-buffer
           #:bind-buffer
           #:unbind-buffer
           #:delete-buffer
           #:write-buffer-path
           #:uniforms
           #:uniform-int
           #:uniform-int-array
           #:uniform-float
           #:uniform-float-array
           #:uniform-vec2
           #:uniform-vec2-array
           #:uniform-vec3
           #:uniform-vec3-array
           #:uniform-vec4
           #:uniform-vec4-array
           #:uniform-mat2
           #:uniform-mat2-array
           #:uniform-mat3
           #:uniform-mat3-array
           #:uniform-mat4
           #:uniform-mat4-array)

;;; shader functions

  ;; common
  (:export #:mvlet*)
  ;; math
  (:export #:+epsilon+
           #:+pi+
           #:+half-pi+
           #:log10
           #:saturate
           #:map-domain)
  ;; graphing
  (:export #:graph)
  ;; color space conversion
  (:export #:rgb->grayscale
           #:hue->rgb
           #:rgb->hcv
           #:rgb->hsv
           #:hsv->rgb
           #:rgb->hcy
           #:hcy->rgb
           #:rgb->hsl
           #:hsl-rgb
           #:rgb->srgb-approx
           #:rgb->srgb
           #:srgb->rgb-approx
           #:srgb->rgb
           #:rgb->xyz
           #:xyz->rgb
           #:xyy->xyz
           #:xyz->xyy
           #:rgb->xyy
           #:xyy->rgb)
  ;; color grading
  (:export #:set-exposure
           #:set-saturation
           #:set-contrast
           #:set-brightness
           #:set-gamma
           #:color-filter
           #:tone-map/linear
           #:tone-map/reinhard
           #:tone-map/haarm-peter-duiker
           #:tone-map/hejl-burgess-dawson
           #:tone-map/uncharted2)
  ;; shaping - penner
  (:export #:linear
           #:sine-out
           #:sine-in
           #:sine-in-out
           #:quadratic-out
           #:quadratic-in
           #:quadratic-in-out
           #:cubic-out
           #:cubic-in
           #:cubic-in-out
           #:quartic-out
           #:quartic-in
           #:quartic-in-out
           #:quintic-out
           #:quintic-in
           #:quintic-in-out
           #:exponential-out
           #:exponential-in
           #:exponential-in-out
           #:circular-out
           #:circular-in
           #:circular-in-out
           #:back-out
           #:back-in
           #:back-in-out
           #:elastic-out
           #:elastic-in
           #:elastic-in-out
           #:bounce-out
           #:bounce-in
           #:bounce-in-out)
  ;; shaping - iq
  (:export #:almost-identity
           #:impulse
           #:cubic-pulse
           #:exponential-step
           #:gain
           #:parabola
           #:power-curve
           #:sinc-curve)
  ;; shaping - levin
  (:export #:exponential-emphasis
           #:double-exponential-seat
           #:double-exponential-sigmoid
           #:logistic-sigmoid
           #:double-circle-seat
           #:double-circle-sigmoid
           #:double-elliptical-seat
           #:double-elliptical-sigmoid
           #:blinn-wyvill-raised-inverted-cosine
           #:double-cubic-seat
           #:double-cubic-seat/linear-blend
           #:double-odd-polynomial-seat
           #:quadratic-point)
  ;; shaping - misc
  (:export #:hermite-curve
           #:quintic-curve
           #:quintic-curve/interpolate-derivative
           #:quintic-curve/derivative
           #:quintic-curve/fast
           #:quintic-hermite
           #:quintic-hermite/derivative
           #:falloff-squared-c1
           #:falloff-squared-c2)
  ;; hashing
  (:export #:blum-blum-shub
           #:blum-blum-shub/hq
           #:sgpp
           #:sgpp/2-per-corner
           #:sgpp/3-per-corner
           #:fast32
           #:fast32/2-per-corner
           #:fast32/3-per-corner
           #:fast32/4-per-corner
           #:fast32/cell
           #:fast32-2
           #:fast32-2/4-per-corner)
  ;; noise
  (:export #:perlin
           #:perlin/derivs
           #:perlin-surflet
           #:perlin-surflet/derivs
           #:perlin-improved
           #:cellular
           #:cellular/derivs
           #:cellular-fast
           #:polkadot
           #:polkadot-box
           #:hermite
           #:hermite/derivs
           #:simplex-perlin
           #:simplex-perlin/derivs
           #:simplex-cellular
           #:simplex-polkadot
           #:value
           #:value/derivs
           #:value-perlin
           #:value-hermite
           #:cubist
           #:stars))
