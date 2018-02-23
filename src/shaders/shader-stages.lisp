(in-package :fl.shaders-new)

(defstruct-gpu texture-struct ()
  (sampler1 :sampler-2d :accessor sampler1)
  (sampler2 :sampler-2d :accessor sampler2))


(defun-gpu default-vertex (;; These become vertex attributes
                           (pos :vec3)
                           (normal :vec3)
                           (tangent :vec3)
                           (color :vec4)
                           (uv1 :vec2)
                           (uv2 :vec2)
                           (joints :vec4)
                           (weights :vec4)
                           &uniform
                           (model :mat4)
                           (view :mat4)
                           (proj :mat4))
  (values
   ;; position is implicitly first
   (* proj view model (vec4 pos 1))
   ;; Then the rest must match the fragment shader lambda-list
   normal
   tangent
   color
   uv1
   uv2))

(defun-gpu color-decal-fragment ((normal :vec3)
                                 (tangent :vec3)
                                 (color :vec4)
                                 (uv1 :vec2)
                                 (uv2 :vec2))
  (if (= (.w color) 0)
      (discard)
      color))

(defun-gpu color-fragment ((normal :vec3)
                           (tangent :vec3)
                           (color :vec4)
                           (uv1 :vec2)
                           (uv2 :vec2))
  color)

(defun-gpu texture-fragment ((normal :vec3)
                             (tangent :vec3)
                             (color :vec4)
                             (uv1 :vec2)
                             (uv2 :vec2)
                             &uniform
                             (tex texture-struct))

  (* (texture (sampler1 tex) uv1) color))


;; test enacting the shaders stuff.....

(initialize-shaders)

(make-shader-program :unlit-color-decal (:version 430 :primitive :triangles)
  (:vertex () (default-vertex :vec3 :vec3 :vec3 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment () (color-decal-fragment :vec3 :vec3 :vec4 :vec2 :vec2)))

#++(make-shader-program :unlit-color (:version 330 :primitive :triangles)
     (:vertex () (default-vertex :vec3 :vec3 :vec3 :vec4 :vec2 :vec2 :vec4 :vec4))
     (:fragment () (color-fragment :vec3 :vec3 :vec4 :vec2 :vec2)))

#++(make-shader-program :unlit-texture (:version 330 :primitive :triangles)
     (:vertex () (default-vertex :vec3 :vec3 :vec3 :vec4 :vec2 :vec2 :vec4 :vec4))
     (:fragment () (texture-fragment :vec3 :vec3 :vec4 :vec2 :vec2)))
