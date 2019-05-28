(in-package #:cl-user)

(defpackage #:first-light.gpu.color
  (:nicknames #:fl.gpu.color)
  (:use #:fl.gpu.lib
        #:fl.gpu.swizzle)
  ;; color space conversion
  (:export
   #:rgb->grayscale
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
  (:export
   #:set-exposure
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
