(in-package :first-light-shaders)

(input pos :vec3 :location 0)
(input uv :vec3 :location 1)

(output frag-color :vec4 :stage :fragment)

(uniform model :mat4)
(uniform view :mat4)
(uniform proj :mat4)

(interface varyings (:out (:vertex v-out)
                     :in (:fragment f-in))
  (uv :vec3))

(defun default-vertex ()
  (setf (@ v-out uv) uv
        gl-position (* proj view model (vec4 pos 1))))

(defun default-fragment ()
  (setf frag-color (vec4 1 1 1 1))
  (when (zerop (.a frag-color))
    (discard)))
