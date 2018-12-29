(in-package :defpackage+-user-1)

(defpackage+ #:first-light.gpu
  (:nicknames #:fl.gpu)
  (:use #:cl)
  (:export #:define-function
           #:define-struct
           #:define-macro
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
           #:uniform-mat4-array))

(defpackage+ #:first-light.gpu.lib
  (:nicknames #:fl.gpu.lib)
  (:import-from #:first-light.gpu
                #:define-function
                #:define-struct
                #:define-macro
                #:define-shader)
  (:import-from #:varjo
                #:v-def-glsl-template-fun
                #:v-float
                #:v-vec2
                #:v-vec3
                #:v-vec4
                #:v-mat4)
  (:inherit #:cl
            #:vari)
  (:export #:define-function
           #:define-struct
           #:define-macro
           #:define-shader
           #:mvlet*))

(defpackage+ #:first-light.gpu.math
  (:nicknames #:fl.gpu.math)
  (:use #:first-light.gpu.lib)
  (:export #:+epsilon+
           #:+pi+
           #:+half-pi+
           #:log10
           #:saturate
           #:map-domain))

(defpackage+ #:first-light.gpu.graph
  (:nicknames #:fl.gpu.graph)
  (:use #:first-light.gpu.lib)
  (:export #:graph))

(defpackage+ #:first-light.gpu.color
  (:nicknames #:fl.gpu.color)
  (:use #:first-light.gpu.lib
        #:first-light.gpu.math)
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
           #:tone-map/uncharted2))

(defpackage+ #:first-light.gpu.shaping
  (:nicknames #:fl.gpu.shaping)
  (:use #:first-light.gpu.lib
        #:first-light.gpu.math)
  ;; penner
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
  ;; iq
  (:export #:almost-identity
           #:impulse
           #:cubic-pulse
           #:exponential-step
           #:gain
           #:parabola
           #:power-curve
           #:sinc-curve)
  ;; levin
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
  ;; misc
  (:export #:hermite-curve
           #:quintic-curve
           #:quintic-curve/interpolate-derivative
           #:quintic-curve/derivative
           #:quintic-curve/fast
           #:quintic-hermite
           #:quintic-hermite/derivative
           #:falloff-squared-c1
           #:falloff-squared-c2))

(defpackage+ #:first-light.gpu.hash
  (:nicknames #:fl.gpu.hash)
  (:use #:first-light.gpu.lib)
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
           #:fast32-2/4-per-corner))

(defpackage+ #:first-light.gpu.noise
  (:nicknames #:fl.gpu.noise)
  (:use #:first-light.gpu.lib
        #:first-light.gpu.math)
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

(defpackage+ #:first-light.gpu.texture
  (:nicknames #:fl.gpu.texture)
  (:use #:first-light.gpu.lib)
  (:export #:unlit-color
           #:unlit-color-decal
           #:unlit-texture
           #:unlit-texture-decal))

(defpackage+ #:first-light.gpu.user
  (:nicknames #:fl.gpu.user)
  (:use #:first-light.gpu.lib
        #:first-light.gpu.math))
