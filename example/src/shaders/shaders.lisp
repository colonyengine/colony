(in-package :first-light-shaders)

(input pos :vec2 :location 0)
(input uv :vec2 :location 1)
(input color :vec4 :location 2)

(output frag-color :vec4 :stage :fragment)

(uniform model :mat4)
(uniform view :mat4)
(uniform proj :mat4)

(interface varyings (:out (:vertex v-out)
                     :in (:fragment f-in))
  (color :vec4)
  (uv :vec2))

(defun unlit-texture-vertex ()
  (setf (@ v-out uv) uv
        (@ v-out color) color
        gl-position (* proj view model (vec4 pos 0 1))))

(defun unlit-texture-fragment ()
  ;; TODO: Fix me to look up a texture and get the right value from it.
  (setf frag-color (@ f-in color))
  (when (zerop (.a frag-color))
    (discard)))
