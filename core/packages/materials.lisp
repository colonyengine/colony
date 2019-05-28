(in-package #:cl-user)

(defpackage #:first-light.materials
  (:nicknames #:fl.materials)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m4 #:box.math.mat4))
  (:use #:cl)
  ;; helper functions
  (:export #:total-time/uniform)
  ;; profiles
  (:export
   #:u-model
   #:u-mvp
   #:u-mvpt
   #:u-proj
   #:u-time
   #:u-view
   #:u-vp
   #:u-vpt))
