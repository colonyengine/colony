(in-package #:cl-user)

(defpackage #:first-light.materials
  (:nicknames #:fl.materials)
  (:local-nicknames (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m4 #:origin.mat4))
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
