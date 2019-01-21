(in-package :first-light.example)

;;; damaged-helmet

(fl:define-material damaged-helmet
  (:shader fl.gpu.user:damaged-helmet
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:metallic-roughness-values (m:vec2 1))
    (:metallic-roughness-sampler 'damaged-helmet/metallic-roughness)
    (:base-color-sampler 'damaged-helmet/color)
    (:base-color-factor (m:vec4 1))
    (:normal-sampler 'damaged-helmet/normal)
    (:normal-scale 1.0)
    ;; NOTE: This vector points TOWARDS the light.
    (:light-direction (m:vec3 0 1 1))
    (:light-color (m:vec3 1))
    (:occlusion-sampler 'damaged-helmet/ambient-occlusion)
    (:occlusion-strength 1.0)
    (:emissive-sampler 'damaged-helmet/emissive)
    (:emissive-factor 0.3))))

;;; graph-test

(fl:define-material graph-test
  (:profiles (fl.materials:u-mvpt)
   :shader fl.gpu.user:graph-test))

(fl:define-material 3d-graph-test
  (:profiles (fl.materials:u-mvpt)
   :shader fl.gpu.user:3d-graph-test/1
   :instances 1000
   :attributes (:depth :always)
   :uniforms
   ((:size 1)
    (:min 0)
    (:by 1))))

;;; noise-test

(fl:define-material noise-test
  (:profiles (fl.materials:u-mvpt)
   :shader fl.gpu.user:noise-test))

;;; texture-test

(fl:define-material texture-test/1d-gradient
  (:shader fl.gpu.user:unlit-texture-1d
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/1d-gradient)
    (:mix-color (m:vec4 1)))))

(fl:define-material texture-test/2d-wood
  (:shader fl.gpu.texture:unlit-texture
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/2d-wood)
    (:mix-color (m:vec4 1)))))

(fl:define-material texture-test/3d-testpat
  (:shader fl.gpu.user:unlit-texture-3d
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/3d-testpat)
    (:mix-color (m:vec4 1))
    (:uv-z (lambda (context material)
             (declare (ignore material))
             ;; make sin in the range of 0 to 1 for texture coord.
             (/ (1+ (sin (* (fl:total-time context) 1.5))) 2.0))))))

(fl:define-material texture-test/1d-array-testpat
  (:shader fl.gpu.user:unlit-texture-1d-array
   :profiles (fl.materials:u-mvpt)
   :uniforms
   ((:tex.sampler1 'texture-test/1d-array-testpat)
    (:mix-color (m:vec4 1))
    (:num-layers 4))))

(fl:define-material texture-test/2d-array-testarray
  (:shader fl.gpu.user:unlit-texture-2d-array
   :profiles (fl.materials:u-mvpt)
   :uniforms
   ((:tex.sampler1 'texture-test/2d-array-testarray)
    (:mix-color (m:vec4 1))
    (:uv-z (lambda (context material)
             (declare (ignore material))
             ;; make sin in the range of 0 to 1 for texture coord.
             (/ (1+ (sin (* (fl:total-time context) 1.5))) 2.0)))
    (:num-layers 4))))

(fl:define-material texture-test/2d-sweep-input
  (:shader fl.gpu.user:noise-2d/sweep-input
   :profiles (fl.materials:u-mvp)
   :uniforms
   ;; any old 2d texture here will do since we overwrite it with noise.
   ((:tex.sampler1 'texture-test/2d-wood)
    (:tex.channel0 (m:vec2))
    (:mix-color (m:vec4 1)))))

(fl:define-material texture-test/testcubemap
  (:shader fl.gpu.user:unlit-texture-cube-map
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/testcubemap)
    (:mix-color (m:vec4 1)))))

(fl:define-material texture-test/testcubemaparray
  (:shader fl.gpu.user:unlit-texture-cube-map-array
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/testcubemaparray)
    (:mix-color (m:vec4 1))
    (:cube-layer (lambda (context material)
                   (declare (ignore material))
                   ;; make sin in the range of 0 to 1 for texture coord.
                   (/ (1+ (sin (* (fl:total-time context) 1.5))) 2.0)))
    (:num-layers 2))))
