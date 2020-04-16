(in-package #:cl-user)

(defpackage #:virality.extensions.materials
  (:use #:cl)
  ;; profiles
  (:export
   #:u-model
   #:u-mvp
   #:u-mvpt
   #:u-mvptr
   #:u-proj
   #:u-time
   #:u-view
   #:u-vp
   #:u-vpt))

(defpackage #:virality.extensions.textures
  (:use #:cl)
  ;; profiles
  (:export
   #:clamp-all-edges
   #:default-profile)
  ;; textures
  (:export
   #:debug-texture))
