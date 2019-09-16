(in-package #:cl-user)

(defpackage #:virality.shaders.texture
  (:use #:cl #:vari #:umbra.common)
  (:export
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal))

(defpackage #:virality.shaders.visualization
  (:use #:cl #:vari #:umbra.common)
  (:export
   #:collider/sphere))
