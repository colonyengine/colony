(in-package #:virality.examples.shaders)

(defstruct dynamic-attrs
  (pos :vec3)
  (normal :vec3)
  (uv :vec3))

(defun dynamic-geometry/vert ((attrs dynamic-attrs)
                              &uniforms
                              (model :mat4)
                              (view :mat4)
                              (proj :mat4))
  (with-slots (pos uv) attrs
    (values (* proj view model (vec4 pos 1))
            uv)))

(defun dynamic-geometry/frag ((uv :vec3))
  (vec4 1 0 0 1))

(define-shader dynamic-geometry ()
  (:vertex (dynamic-geometry/vert dynamic-attrs))
  (:fragment (dynamic-geometry/frag :vec3)))
