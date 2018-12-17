(in-package :defpackage+-user-1)

(defpackage+ #:fl.shaderlib
  (:use #:cl)
  (:export #:define-gpu-function
           #:define-gpu-struct
           #:define-gpu-macro
           #:enable-dependency-tracking
           #:disable-dependency-tracking
           #:reset-program-state
           #:define-shader
           #:find-program
           #:view-source
           #:build-shader-program
           #:build-shader-programs
           #:build-shader-dictionary
           #:translate-shader-programs
           #:set-modify-hook
           #:create-block-alias
           #:find-block
           #:bind-block
           #:unbind-block
           #:rebind-blocks
           #:buffer-name
           #:find-buffer
           #:create-buffer
           #:bind-buffer
           #:unbind-buffer
           #:delete-buffer
           #:write-buffer-path
           #:with-shader-program
           #:uniforms
           #:uniform-int
           #:uniform-int-array
           #:uniform-float
           #:uniform-float-array
           #:uniform-vec2
           #:uniform-vec2-array
           #:uniform-vec3
           #:uniform-vec3-array
           #:uniform-vec4
           #:uniform-vec4-array
           #:uniform-mat2
           #:uniform-mat2-array
           #:uniform-mat3
           #:uniform-mat3-array
           #:uniform-mat4
           #:uniform-mat4-array))

(defpackage+ #:fl.shaderlib.vari
  (:inherit #:cl
            #:vari)
  (:import-from #:varjo
                #:v-def-glsl-template-fun
                #:v-float
                #:v-vec2
                #:v-vec3
                #:v-vec4
                #:v-mat4))
