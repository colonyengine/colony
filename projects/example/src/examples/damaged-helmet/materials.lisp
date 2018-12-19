(in-package :first-light.example)

(define-material damaged-helmet
  (:shader fl.shader:damaged-helmet
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:metallic-roughness-values (flm:vec2 1))
    (:metallic-roughness-sampler 'damaged-helmet/metallic-roughness)
    (:base-color-sampler 'damaged-helmet/color)
    (:base-color-factor (flm:vec4 1))
    (:normal-sampler 'damaged-helmet/normal)
    (:normal-scale 1.0)
    ;; NOTE: This vector points TOWARDS the light.
    (:light-direction (flm:vec3 0 1 1))
    (:light-color (flm:vec3 1))
    (:occlusion-sampler 'damaged-helmet/ambient-occlusion)
    (:occlusion-strength 1.0)
    (:emissive-sampler 'damaged-helmet/emissive)
    (:emissive-factor 0.3))))
