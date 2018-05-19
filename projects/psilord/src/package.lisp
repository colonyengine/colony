(in-package :defpackage+-user-1)

(defpackage+ #:fl.psilord
  (:use #:cl
        #:fl.core
        #:fl.comp.mesh-renderer)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:export-only #:demo
                #:material-test))

(defpackage+ #:fl.psilord.shaders
  (:use #:cl #:shadow)
  (:inherit #:box.math.vari #:vari)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:export-only #:sprite-shader))

(defpackage+ #:fl.psilord.materials
  (:use #:cl #:shadow)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:import-from #:fl.core
                #:define-material)
  (:export-only #:sprite))

(defpackage+ #:fl.psilord.textures
  (:use #:cl #:shadow)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:import-from #:fl.core
                #:define-texture
                #:general-data-format-descriptor)
  (:export-only #:sprite-sheet-00))
