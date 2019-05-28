(in-package #:cl-user)

(defpackage #:first-light.gpu.shaping
  (:nicknames #:fl.gpu.shaping)
  (:use #:fl.gpu.lib
        #:fl.gpu.swizzle)
  ;; penner
  (:export
   #:linear
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
  (:export
   #:almost-identity
   #:impulse
   #:cubic-pulse
   #:exponential-step
   #:gain
   #:parabola
   #:power-curve
   #:sinc-curve)
  ;; levin
  (:export
   #:exponential-emphasis
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
  (:export
   #:hermite-curve
   #:quintic-curve
   #:quintic-curve/interpolate-derivative
   #:quintic-curve/derivative
   #:quintic-curve/fast
   #:quintic-hermite
   #:quintic-hermite/derivative
   #:falloff-squared-c1
   #:falloff-squared-c2))
