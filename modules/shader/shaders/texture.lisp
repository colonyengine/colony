(in-package :first-light.shader)

(defstruct texture-struct
  (sampler1 :sampler-2d :accessor sampler1)
  (sampler2 :sampler-2d :accessor sampler2))

(defun unlit/vert ((pos :vec3)
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
          color
          uv1))

(defun unlit/vert-only-uv1 ((pos :vec3)
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

(defun unlit-color/frag ((color :vec4)
                         (uv1 :vec2))
  color)


(defun unlit-color-decal/frag ((color :vec4)
                               (uv1 :vec2))
  (if (zerop (.a color))
      (discard)
      color))

(defun unlit-texture/frag ((color :vec4)
                           (uv1 :vec2)
                           &uniform
                           (tex texture-struct)
                           (mix-color :vec4))
  (let ((tex-color (texture (sampler1 tex) uv1)))
    (* tex-color mix-color)))

(defun make-fragment-filter ((filter (function (:vec4) :bool)))
  (lambda ((color :vec4))
    (if (funcall filter color)
        color
        (discard))))

(defun unlit-texture-decal/frag ((color :vec4)
                                 (uv1 :vec2)
                                 &uniform
                                 (min-intensity :vec4)
                                 (max-intensity :vec4)
                                 (tex texture-struct))
  (let ((frag-filter (make-fragment-filter
                      (lambda ((color :vec4))
                        (and
                         (and (>= (.r color) (.r min-intensity))
                              (>= (.g color) (.g min-intensity))
                              (>= (.b color) (.b min-intensity))
                              (>= (.a color) (.a min-intensity)))
                         (and (<= (.r color) (.r max-intensity))
                              (<= (.g color) (.g max-intensity))
                              (<= (.b color) (.b max-intensity))
                              (<= (.a color) (.a max-intensity)))))))
        (color (texture (sampler1 tex) uv1)))
    (funcall frag-filter color)))

(define-shader unlit-color ()
  (:vertex (unlit/vert :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (unlit-color/frag :vec4 :vec2)))

(define-shader unlit-color-decal ()
  (:vertex (unlit/vert :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (unlit-color-decal/frag :vec4 :vec2)))

(define-shader unlit-texture ()
  (:vertex (unlit/vert :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (unlit-texture/frag :vec4 :vec2)))

(define-shader unlit-texture-decal ()
  (:vertex (unlit/vert :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (unlit-texture-decal/frag :vec4 :vec2)))
