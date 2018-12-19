(in-package :first-light.example)

(define-material texture-test/1d-gradient
  (:shader fl.shader:unlit-texture-1d
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/1d-gradient)
    (:mix-color (flm:vec4 1)))))

(define-material texture-test/2d-wood
  (:shader fl.shader:unlit-texture
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/2d-wood)
    (:mix-color (flm:vec4 1)))))

(define-material texture-test/3d-testpat
  (:shader fl.shader:unlit-texture-3d
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/3d-testpat)
    (:mix-color (flm:vec4 1))
    (:uv-z (lambda (context material)
             (declare (ignore material))
             ;; make sin in the range of 0 to 1 for texture coord.
             (/ (1+ (sin (* (total-time context) 1.5))) 2.0))))))

(define-material texture-test/1d-array-testpat
  (:shader fl.shader:unlit-texture-1d-array
   :profiles (fl.materials:u-mvpt)
   :uniforms
   ((:tex.sampler1 'texture-test/1d-array-testpat)
    (:mix-color (flm:vec4 1))
    (:num-layers 4))))

(define-material texture-test/2d-array-testarray
  (:shader fl.shader:unlit-texture-2d-array
   :profiles (fl.materials:u-mvpt)
   :uniforms
   ((:tex.sampler1 'texture-test/2d-array-testarray)
    (:mix-color (flm:vec4 1))
    (:uv-z (lambda (context material)
             (declare (ignore material))
             ;; make sin in the range of 0 to 1 for texture coord.
             (/ (1+ (sin (* (total-time context) 1.5))) 2.0)))
    (:num-layers 4))))

(define-material texture-test/2d-sweep-input
  (:shader fl.shader:noise-2d/sweep-input
   :profiles (fl.materials:u-mvp)
   :uniforms
   ;; any old 2d texture here will do since we overwrite it with noise.
   ((:tex.sampler1 'texture-test/2d-wood)
    (:tex.channel0 (flm:vec2))
    (:mix-color (flm:vec4 1)))))

(define-material texture-test/testcubemap
  (:shader fl.shader:unlit-texture-cube-map
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/testcubemap)
    (:mix-color (flm:vec4 1)))))

(define-material texture-test/testcubemaparray
  (:shader fl.shader:unlit-texture-cube-map-array
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'texture-test/testcubemaparray)
    (:mix-color (flm:vec4 1))
    (:cube-layer (lambda (context material)
                   (declare (ignore material))
                   ;; make sin in the range of 0 to 1 for texture coord.
                   (/ (1+ (sin (* (total-time context) 1.5))) 2.0)))
    (:num-layers 2))))
