(in-package :first-light.gpu.user)

(define-function noise-test/frag ((uv :vec2)
                                  &uniform
                                  (time :float))
  (vec4 uv time 1))

(define-function noise-test/perlin-3d/frag ((uv :vec2)
                                            &uniform
                                            (time :float))
  (let ((noise (vec3 (fl.gpu.noise:perlin (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/perlin-surflet-3d/frag ((uv :vec2)
                                                    &uniform
                                                    (time :float))
  (let ((noise (vec3 (fl.gpu.noise:perlin-surflet (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/perlin-improved-3d/frag ((uv :vec2)
                                                     &uniform
                                                     (time :float))
  (let ((noise (vec3 (fl.gpu.noise:perlin-improved (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/perlin-4d/frag ((uv :vec2)
                                            &uniform
                                            (time :float))
  (let ((noise (vec3 (fl.gpu.noise:perlin (vec4 (* 10 uv) time (/ time 2))))))
    (vec4 noise 1)))

(define-function noise-test/cellular-3d/frag ((uv :vec2)
                                              &uniform
                                              (time :float))
  (let ((noise (vec3 (fl.gpu.noise:cellular (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/cellular-fast-3d/frag ((uv :vec2)
                                                   &uniform
                                                   (time :float))
  (let ((noise (vec3 (fl.gpu.noise:cellular-fast (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/hermite-3d/frag ((uv :vec2)
                                             &uniform
                                             (time :float))
  (let ((noise (vec3 (fl.gpu.noise:hermite (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/simplex-perlin-3d/frag ((uv :vec2)
                                                    &uniform
                                                    (time :float))
  (let ((noise (vec3 (fl.gpu.noise:simplex-perlin (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/simplex-cellular-3d/frag ((uv :vec2)
                                                      &uniform
                                                      (time :float))
  (let ((noise (vec3 (fl.gpu.noise:simplex-cellular (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/simplex-polkadot-3d/frag ((uv :vec2)
                                                      &uniform
                                                      (time :float))
  (let ((noise (vec3 (fl.gpu.noise:simplex-polkadot (vec3 (* 10 uv) time) 1.0 1.0))))
    (vec4 noise 1)))

(define-function noise-test/value-3d/frag ((uv :vec2)
                                           &uniform
                                           (time :float))
  (let ((noise (vec3 (fl.gpu.noise:value (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/value-4d/frag ((uv :vec2)
                                           &uniform
                                           (time :float))
  (let ((noise (vec3 (fl.gpu.noise:value (vec4 (* 10 uv) time (/ time 2))))))
    (vec4 noise 1)))

(define-function noise-test/hermite-3d/frag ((uv :vec2)
                                             &uniform
                                             (time :float))
  (let ((noise (vec3 (fl.gpu.noise:hermite (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(define-function noise-test/value-hermite-3d/frag ((uv :vec2)
                                                   &uniform
                                                   (time :float))
  (let ((noise (vec3 (fl.gpu.noise:value-hermite (vec3 (* 10 uv) time) 0.5 0.5 1.0))))
    (vec4 noise 1)))

(define-function noise-test/value-perlin-3d/frag ((uv :vec2)
                                                  &uniform
                                                  (time :float))
  (let ((noise (vec3 (fl.gpu.noise:value-perlin (vec3 (* 10 uv) time) 0.5))))
    (vec4 noise 1)))

(define-function noise-test/polkadot-3d/frag ((uv :vec2)
                                              &uniform
                                              (time :float))
  (let ((noise (vec3 (fl.gpu.noise:polkadot (vec3 (* 10 uv) time) 0.0 1.0))))
    (vec4 noise 1)))

(define-function noise-test/polkadot-box-3d/frag ((uv :vec2)
                                                  &uniform
                                                  (time :float))
  (let ((noise (vec3 (fl.gpu.noise:polkadot-box (vec3 (* 10 uv) time) 0.0 1.0))))
    (vec4 noise 1)))

(define-function noise-test/cubist-3d/frag ((uv :vec2)
                                            &uniform
                                            (time :float))
  (let ((noise (vec3 (fl.gpu.noise:cubist (vec3 (* 10 uv) time) (vec2 0 1.0)))))
    (vec4 noise 1)))

(define-shader noise-test (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/frag :vec2)))

(define-shader noise-test/perlin-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/perlin-3d/frag :vec2)))

(define-shader noise-test/perlin-surflet-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/perlin-surflet-3d/frag :vec2)))

(define-shader noise-test/perlin-improved-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/perlin-improved-3d/frag :vec2)))

(define-shader noise-test/perlin-4d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/perlin-4d/frag :vec2)))

(define-shader noise-test/cellular-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/cellular-3d/frag :vec2)))

(define-shader noise-test/cellular-fast-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/cellular-fast-3d/frag :vec2)))

(define-shader noise-test/hermite-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/hermite-3d/frag :vec2)))

(define-shader noise-test/simplex-perlin-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/simplex-perlin-3d/frag :vec2)))

(define-shader noise-test/simplex-cellular-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/simplex-cellular-3d/frag :vec2)))

(define-shader noise-test/simplex-polkadot-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/simplex-polkadot-3d/frag :vec2)))

(define-shader noise-test/value-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/value-3d/frag :vec2)))

(define-shader noise-test/value-4d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/value-4d/frag :vec2)))

(define-shader noise-test/value-hermite-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/value-hermite-3d/frag :vec2)))

(define-shader noise-test/value-perlin-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/value-perlin-3d/frag :vec2)))

(define-shader noise-test/polkadot-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/polkadot-3d/frag :vec2)))

(define-shader noise-test/polkadot-box-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/polkadot-box-3d/frag :vec2)))

(define-shader noise-test/cubist-3d (:version 430)
  (:vertex (fl.gpu.texture:unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/cubist-3d/frag :vec2)))
