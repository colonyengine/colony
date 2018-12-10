(in-package :defpackage+-user-1)

(defpackage+ #:fl.textures
  (:use #:cl #:%fl)
  ;; profiles
  (:export #:clamp-all-edges
           #:default-profile
           ;; TODO: exploratory framebuffer support
           #:framebuffer)
  ;; textures
  (:export #:debug-texture
           #:missing-texture
           ;; TODO: Exploratory framebuffer support
           #:framebuffer-color
           #:framebuffer-depth
           #:framebuffer-depth/stencil
           #:framebuffer-stencil))
