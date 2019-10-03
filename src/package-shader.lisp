(in-package #:cl-user)

(defpackage #:virality.shaders.texture
  (:use #:shadow.glsl #:umbra.common)
  (:export
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal))

(defpackage #:virality.shaders.visualization
  (:use #:shadow.glsl #:umbra.common)
  (:export
   #:collider/sphere))
