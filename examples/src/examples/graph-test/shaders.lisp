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

;;;

(define-function 3d-graph-test/graph ((fn (function (:float) :vec3))
                                      (pos :vec3)
                                      (view :mat4)
                                      (proj :mat4)
                                      (size :float)
                                      (min :float)
                                      (by :float))
  (mvlet* ((input (+ min (* by (float gl-instance-id))))
           (result color (funcall fn input))
           (model (vec4 result 1))
           (pos (+ (* view model)
                   (vec4 (* pos size) 0))))
    (values (* proj pos)
            color)))

(define-function 3d-graph-test/test-1 ((i :float)
                                       (time :float))
  (let* ((offset (vec3 0 0 (* i 0.002)))
         (m3 (mat3 (cos i) 0 (- (sin i))
                   0 1 0
                   (sin i) 0 (cos i)))
         (pos (* m3 offset))
         (h (* 40 (fl.gpu.noise:perlin-surflet (+ (* 0.03 pos)
                                                  (vec3 (* 0.5 time))))))
         (pos (vec3 (.x pos) h (.z pos)))
         (color (mix (vec4 1 0.8 0 0)
                     (vec4 0.85)
                     h)))
    (values pos color)))

(define-function 3d-graph-test/test-2 ((i :float)
                                       (time :float))
  (let* ((dist (* i 0.02))
         (h (* 5 (sin (* 0.005 (+ i (* 400 time))))))
         (offset (vec3 0 h dist))
         (r (* i 0.2))
         (m3 (mat3 (cos r) 0 (- (sin r))
                   0 1 0
                   (sin r) 0 (cos r)))
         (pos (* m3 offset))
         (color (vec4 0 0.6 0.85 1)))
    (values pos color)))

(define-function 3d-graph-test/vert1 ((mesh-attrs mesh-attrs)
                                      &uniform
                                      (model :mat4)
                                      (view :mat4)
                                      (proj :mat4)
                                      (size :float)
                                      (min :float)
                                      (by :float)
                                      (time :float))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (let ((fn (lambda ((i :float))
                (3d-graph-test/test-1 i time))))
      (mvlet* ((pos color (3d-graph-test/graph fn mesh/pos view proj size min by)))
        (values pos
                mesh/uv1
                color)))))

(define-function 3d-graph-test/vert2 ((mesh-attrs mesh-attrs)
                                      &uniform
                                      (model :mat4)
                                      (view :mat4)
                                      (proj :mat4)
                                      (size :float)
                                      (min :float)
                                      (by :float)
                                      (time :float))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (let ((fn (lambda ((i :float))
                (3d-graph-test/test-2 i time))))
      (mvlet* ((pos color (3d-graph-test/graph fn mesh/pos view proj size min by)))
        (values pos
                mesh/uv1
                color)))))

(define-function 3d-graph-test/frag ((uv :vec2)
                                     (color :vec4)
                                     &uniform
                                     (time :float))
  (let ((scale 1))
    (mix (vec4 0)
         color
         (fl.gpu.sdf:mask/fill
          (fl.gpu.sdf:dist/circle (- (* uv 2 scale) (vec2 scale))
                                  scale)))))

(define-shader 3d-graph-test/1 (:version 430)
  (:vertex (3d-graph-test/vert1 mesh-attrs))
  (:fragment (3d-graph-test/frag :vec2 :vec4)))

(define-shader 3d-graph-test/2 (:version 430)
  (:vertex (3d-graph-test/vert2 mesh-attrs))
  (:fragment (3d-graph-test/frag :vec2 :vec4)))
