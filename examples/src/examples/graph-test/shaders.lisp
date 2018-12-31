(in-package :first-light.gpu.user)

(define-function graph-test/frag ((uv :vec2)
                                  &uniform
                                  (time :float))
  (let* ((dim (vec2 (1+ (sin time)) (+ 2 (sin time))))
         (uv (+ (* uv (- (.y dim) (.x dim)))
                (vec2 (.x dim) -0.5))))
    (fl.gpu.graph:graph
     (lambda ((x :float))
       (* (sin (* x x x)) (sin x)))
     (* 4 uv)
     (vec4 0 1 0 0.5)
     (vec4 1 1 1 0.02)
     10)))

(define-shader graph-test (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 mesh-attrs))
  (:fragment (graph-test/frag :vec2)))
