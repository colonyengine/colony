(in-package :defpackage+-user-1)

(defpackage+ #:fl.shaders
  (:use #:cl #:shadow)
  (:inherit #:box.math.vari #:vari)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat))
  (:export-only #:unlit-color
                #:unlit-color-decal
                #:unlit-texture
                #:unlit-texture-decal
                #:pbr-damaged-helmet))
