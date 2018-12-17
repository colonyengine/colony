(in-package :first-light.shader)

(defun test-shader-0/vert ((pos :vec3)
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
          normal
          tangent
          color
          uv1
          uv2))

(defun test-shader-0/frag ((normal :vec3)
                           (tangent :vec4)
                           (color :vec4)
                           (uv1 :vec2)
                           (uv2 :vec2)
                           &uniform
                           (texes (:sampler-2d 2))
                           (mix-color :vec4)
                           (interpolation (:float 4)))
  (let ((c0 (texture (aref texes 0) uv1))
        (c1 (texture (aref texes 1) uv1)))
    (* (vec4 (mix (.r c0) (.r c1) (aref interpolation 0))
             (mix (.g c0) (.g c1) (aref interpolation 1))
             (mix (.b c0) (.b c1) (aref interpolation 2))
             (mix (.a c0) (.a c1) (aref interpolation 3)))
       mix-color)))

(define-shader test-shader-0 ()
  (:vertex (test-shader-0/vert :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (test-shader-0/frag :vec3 :vec4 :vec4 :vec2 :vec2)))
