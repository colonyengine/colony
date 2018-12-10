(in-package :defpackage+-user-1)

(defpackage+ #:fl.materials
  (:use #:cl #:%fl)
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
