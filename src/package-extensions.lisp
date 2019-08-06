(in-package #:cl-user)

(defpackage #:virality.extensions.actions
  (:use #:cl)
  (:export
   #:fade-in
   #:fade-out
   #:rotate
   #:rotate/reverse
   #:sprite-animate))

(defpackage #:virality.extensions.materials
  (:use #:cl)
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

(defpackage #:virality.extensions.textures
  (:use #:cl)
  ;; profiles
  (:export
   #:clamp-all-edges
   #:default-profile
   ;; TODO: exploratory framebuffer support
   #:framebuffer)
  ;; textures
  (:export
   #:debug-texture
   ;; TODO: Exploratory framebuffer support
   #:framebuffer-color
   #:framebuffer-depth
   #:framebuffer-depth/stencil
   #:framebuffer-stencil))
