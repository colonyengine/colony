(in-package :defpackage+-user-1)

(defpackage+ #:first-light.materials
  (:nicknames #:fl.materials)
  (:use #:cl #:%first-light)
  ;; helper functions
  (:export #:total-time/uniform)
  ;; profiles
  (:export #:u-model
           #:u-mvp
           #:u-mvpt
           #:u-proj
           #:u-time
           #:u-view
           #:u-vp
           #:u-vpt)
  ;; materials
  (:export #:missing-material
           #:unlit-color
           #:unlit-color-decal
           #:unlit-texture
           #:unlit-texture-decal
           #:unlit-texture-decal-bright))
