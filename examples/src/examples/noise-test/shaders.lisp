(in-package :first-light.shader)

(defun noise-test/frag ((uv :vec2)
                        &uniform
                        (time :float))
  (vec4 uv time 1))

(defun noise-test/perlin-3d/frag ((uv :vec2)
                                  &uniform
                                  (time :float))
  (let ((noise (vec3 (perlin (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/perlin-surflet-3d/frag ((uv :vec2)
                                          &uniform
                                          (time :float))
  (let ((noise (vec3 (perlin-surflet (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/perlin-improved-3d/frag ((uv :vec2)
                                           &uniform
                                           (time :float))
  (let ((noise (vec3 (perlin-improved (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/perlin-4d/frag ((uv :vec2)
                                  &uniform
                                  (time :float))
  (let ((noise (vec3 (perlin (vec4 (* 10 uv) time (/ time 2))))))
    (vec4 noise 1)))

(defun noise-test/cellular-3d/frag ((uv :vec2)
                                    &uniform
                                    (time :float))
  (let ((noise (vec3 (cellular (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/cellular-fast-3d/frag ((uv :vec2)
                                         &uniform
                                         (time :float))
  (let ((noise (vec3 (cellular-fast (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/hermite-3d/frag ((uv :vec2)
                                   &uniform
                                   (time :float))
  (let ((noise (vec3 (hermite (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/simplex-perlin-3d/frag ((uv :vec2)
                                          &uniform
                                          (time :float))
  (let ((noise (vec3 (simplex-perlin (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/simplex-cellular-3d/frag ((uv :vec2)
                                            &uniform
                                            (time :float))
  (let ((noise (vec3 (simplex-cellular (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/simplex-polkadot-3d/frag ((uv :vec2)
                                            &uniform
                                            (time :float))
  (let ((noise (vec3 (simplex-polkadot (vec3 (* 10 uv) time) 1.0 1.0))))
    (vec4 noise 1)))

(defun noise-test/value-3d/frag ((uv :vec2)
                                 &uniform
                                 (time :float))
  (let ((noise (vec3 (value (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/value-4d/frag ((uv :vec2)
                                 &uniform
                                 (time :float))
  (let ((noise (vec3 (value (vec4 (* 10 uv) time (/ time 2))))))
    (vec4 noise 1)))

(defun noise-test/hermite-3d/frag ((uv :vec2)
                                   &uniform
                                   (time :float))
  (let ((noise (vec3 (hermite (vec3 (* 10 uv) time)))))
    (vec4 noise 1)))

(defun noise-test/value-hermite-3d/frag ((uv :vec2)
                                         &uniform
                                         (time :float))
  (let ((noise (vec3 (value-hermite (vec3 (* 10 uv) time) 0.5 0.5 1.0))))
    (vec4 noise 1)))

(defun noise-test/value-perlin-3d/frag ((uv :vec2)
                                        &uniform
                                        (time :float))
  (let ((noise (vec3 (value-perlin (vec3 (* 10 uv) time) 0.5))))
    (vec4 noise 1)))

(defun noise-test/polkadot-3d/frag ((uv :vec2)
                                    &uniform
                                    (time :float))
  (let ((noise (vec3 (polkadot (vec3 (* 10 uv) time) 0.0 1.0))))
    (vec4 noise 1)))

(defun noise-test/polkadot-box-3d/frag ((uv :vec2)
                                        &uniform
                                        (time :float))
  (let ((noise (vec3 (polkadot-box (vec3 (* 10 uv) time) 0.0 1.0))))
    (vec4 noise 1)))

(defun noise-test/cubist-3d/frag ((uv :vec2)
                                  &uniform
                                  (time :float))
  (let ((noise (vec3 (cubist (vec3 (* 10 uv) time) (vec2 0 1.0)))))
    (vec4 noise 1)))

(define-shader noise-test (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/frag :vec2)))

(define-shader noise-test/perlin-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/perlin-3d/frag :vec2)))

(define-shader noise-test/perlin-surflet-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/perlin-surflet-3d/frag :vec2)))

(define-shader noise-test/perlin-improved-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/perlin-improved-3d/frag :vec2)))

(define-shader noise-test/perlin-4d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/perlin-4d/frag :vec2)))

(define-shader noise-test/cellular-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/cellular-3d/frag :vec2)))

(define-shader noise-test/cellular-fast-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/cellular-fast-3d/frag :vec2)))

(define-shader noise-test/hermite-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/hermite-3d/frag :vec2)))

(define-shader noise-test/simplex-perlin-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/simplex-perlin-3d/frag :vec2)))

(define-shader noise-test/simplex-cellular-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/simplex-cellular-3d/frag :vec2)))

(define-shader noise-test/simplex-polkadot-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/simplex-polkadot-3d/frag :vec2)))

(define-shader noise-test/value-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/value-3d/frag :vec2)))

(define-shader noise-test/value-4d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/value-4d/frag :vec2)))

(define-shader noise-test/value-hermite-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/value-hermite-3d/frag :vec2)))

(define-shader noise-test/value-perlin-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/value-perlin-3d/frag :vec2)))

(define-shader noise-test/polkadot-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/polkadot-3d/frag :vec2)))

(define-shader noise-test/polkadot-box-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/polkadot-box-3d/frag :vec2)))

(define-shader noise-test/cubist-3d (:version 430)
  (:vertex (unlit/vert-only-uv1 :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (noise-test/cubist-3d/frag :vec2)))
