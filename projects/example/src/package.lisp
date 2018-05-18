(in-package :defpackage+-user-1)

(defpackage+ #:fl.example
  (:use #:cl #:fl.core)
  (:local-nicknames (#:v3 #:box.math.vec3)))

(defpackage+ #:fl.example.shaders
  (:use #:cl #:shadow #:box.math.vari))

(defpackage+ #:fl.example.materials
  (:use #:cl #:shadow)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:import-from #:fl.core
                #:define-material)
  (:export-only #:pbr-damaged-helmet))

(defpackage+ #:fl.example.textures
  (:use #:cl #:shadow)
  (:import-from #:fl.core
                #:define-texture)
  (:export-only #:damaged-helmet/metallic-roughness
                #:damaged-helmet/color
                #:damaged-helmet/normal
                #:damaged-helmet/ambient-occlusion
                #:damaged-helmet/emissive))
