(in-package :fl.shaders)

(initialize-shaders)

(defstruct-gpu texture-struct ()
  (sampler1 :sampler-2d :accessor sampler1)
  (sampler2 :sampler-2d :accessor sampler2))

(defun-gpu vert/default ((pos :vec3)
                         (normal :vec3)
                         (tangent :vec4)
                         (color :vec4)
                         (uv1 :vec2)
                         (uv2 :vec2)
                         (joints :vec4)
                         (weights :vec4)
                         &uniform
                         (model :mat4)
                         (view :mat4)
                         (proj :mat4))
  (values (* proj view model (vec4 pos 1))
          uv1))

(defun-gpu frag/texture ((uv :vec2) &uniform (tex texture-struct))
  (let ((color (texture (sampler1 tex) uv)))
    (if (= (.w color) 0)
        (discard)
        color)))

(make-shader-program :texture (:version 430 :primitive :triangles)
  (:vertex () (vert/default :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment () (frag/texture :vec2)))
