(in-package :fl.example)

;;; Textures

(fl:define-texture damaged-helmet/metallic-roughness (:texture-2d)
  (:data #((:damaged-helmet-textures "metal-roughness.tiff"))))

(fl:define-texture damaged-helmet/color (:texture-2d)
  (:data #((:damaged-helmet-textures "albedo.tiff"))))

(fl:define-texture damaged-helmet/normal (:texture-2d)
  (:data #((:damaged-helmet-textures "normal.tiff"))))

(fl:define-texture damaged-helmet/ambient-occlusion (:texture-2d)
  (:data #((:damaged-helmet-textures "ao.tiff"))))

(fl:define-texture damaged-helmet/emissive (:texture-2d)
  (:data #((:damaged-helmet-textures "emissive.tiff"))))

;;; Materials

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

;;; Prefabs

(fl:define-prefab "damaged-helmet" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (fl.comp:camera (:policy :new-args) :zoom 10))
  (("helmet" :copy "/mesh")
   (fl.comp:transform :rotate (m:vec3 (/ pi 2) 0 0)
                      :rotate/inc (m:vec3 0 0 -0.6)
                      :scale (m:vec3 4))
   (fl.comp:mesh :location '(:mesh "damaged-helmet.glb"))))
