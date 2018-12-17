(in-package :first-light.shader)

(defun noise-test/playground/frag ((uv :vec2)
                                   &uniform
                                   (time :float))
  (let ((noise (vec3 (+ (* 0.23 (perlin (vec3 (* 10 uv) time)))
                        (* 0.11 (perlin (vec3 (* 40 uv) (* 2 time))))
                        (* 0.07 (simplex-perlin (vec3 (* 40 uv) (* 0.5 time))))
                        (* 0.25 (cellular-fast (vec3 (* uv 20) time)))))))
    (mat2x2 1 2 3 4)
    (mix (hsv->rgb (vec4 0.53 0.87 0.76 1))
         (vec4 noise 1)
         0.5)))

(define-shader noise-test/playground (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/playground/frag :vec2)))
