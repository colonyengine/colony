(in-package #:cl-user)

(defpackage #:first-light.textures
  (:nicknames #:fl.textures)
  (:local-nicknames (#:v4 #:origin.vec4))
  (:use #:cl #:%first-light)
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
