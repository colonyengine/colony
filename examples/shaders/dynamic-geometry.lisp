(in-package #:first-light.shader.user)

(define-struct dynamic-attrs
  (pos :vec3)
  (normal :vec3)
  (uv :vec3))

(define-function dynamic-geometry/vert ((attrs dynamic-attrs)
                                        &uniform
                                        (model :mat4)
                                        (view :mat4)
                                        (proj :mat4))
  (with-slots (pos uv) attrs
    (values (* proj view model (vec4 pos 1))
            uv)))

(define-function dynamic-geometry/frag ((uv :vec3))
  (vec4 1 0 0 1))

(define-shader dynamic-geometry ()
  (:vertex (dynamic-geometry/vert dynamic-attrs))
  (:fragment (dynamic-geometry/frag :vec3)))
