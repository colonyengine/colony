(in-package #:first-light.gpu.user)

(define-function starfield/frag ((color :vec4)
                                 (uv1 :vec2)
                                 &uniform
                                 (tex :sampler-2d)
                                 (time :float)
                                 (mix-color :vec4))
  (let ((tex-color (texture tex (vec2 (.x uv1) (- (.y uv1) (/ time 50.0))))))
    (* tex-color mix-color)))


(define-shader starfield ()
  (:vertex (fl.gpu.texture:unlit/vert fl.gpu.lib:mesh-attrs))
  (:fragment (starfield/frag :vec4 :vec2)))
