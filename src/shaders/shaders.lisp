(in-package :fl.shader)

(input pos :vec3 :location 0)
(input normal :vec3 :location 1)
(input tangent :vec4 :location 2)
(input color :vec4 :location 3)
(input uv1 :vec2 :location 4)
(input uv2 :vec2 :location 5)
(input joints :vec4 :location 6)
(input weights :vec4 :location 7)

(output frag-color :vec4 :stage :fragment)

(defstruct texture-struct
  (sampler1 :sampler-2d)
  (sampler2 :sampler-2d))

(uniform model :mat4)
(uniform view :mat4)
(uniform proj :mat4)
(uniform tex texture-struct)

(interface varyings (:out (:vertex v-out)
                     :in (:fragment f-in))
  (color :vec4)
  (uv1 :vec2))

(defun default-vertex ()
  (setf (@ v-out uv1) uv1
        (@ v-out color) color
        gl-position (* proj view model (vec4 pos 1))))

(defun color-fragment ()
  (setf frag-color (@ f-in color))
  (when (zerop (.a frag-color))
    (discard)))

(defun texture-fragment ()
  (setf frag-color (texture (@ tex sampler1) (@ f-in uv1)))
  (when (zerop (.a frag-color))
    (discard)))
