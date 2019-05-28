(in-package #:first-light.gpu.texture)

(define-struct texture-struct
  (sampler1 :sampler-2d :accessor sampler1)
  (sampler2 :sampler-2d :accessor sampler2))

(define-function unlit/vert ((mesh-attrs mesh-attrs)
                             &uniform
                             (model :mat4)
                             (view :mat4)
                             (proj :mat4))
  (with-slots (mesh/pos mesh/color mesh/uv1) mesh-attrs
    (values (* proj view model (vec4 mesh/pos 1))
            mesh/color
            mesh/uv1)))

(define-function unlit/vert-only-uv1 ((mesh-attrs mesh-attrs)
                                      &uniform
                                      (model :mat4)
                                      (view :mat4)
                                      (proj :mat4))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (values (* proj view model (vec4 mesh/pos 1))
            mesh/uv1)))

(define-function unlit-color/frag ((color :vec4)
                                   (uv1 :vec2))
  color)


(define-function unlit-color-decal/frag ((color :vec4)
                                         (uv1 :vec2))
  (if (zerop (.a color))
      (discard)
      color))

(define-function unlit-texture/frag ((color :vec4)
                                     (uv1 :vec2)
                                     &uniform
                                     (tex texture-struct)
                                     (mix-color :vec4))
  (let ((tex-color (texture (sampler1 tex) uv1)))
    (* tex-color mix-color)))

(define-function make-fragment-filter ((filter (function (:vec4) :bool)))
  (lambda ((color :vec4))
    (if (funcall filter color)
        color
        (discard))))

(define-function unlit-texture-decal/frag ((color :vec4)
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
  (:vertex (unlit/vert mesh-attrs))
  (:fragment (unlit-color/frag :vec4 :vec2)))

(define-shader unlit-color-decal ()
  (:vertex (unlit/vert mesh-attrs))
  (:fragment (unlit-color-decal/frag :vec4 :vec2)))

(define-shader unlit-texture ()
  (:vertex (unlit/vert mesh-attrs))
  (:fragment (unlit-texture/frag :vec4 :vec2)))

(define-shader unlit-texture-decal ()
  (:vertex (unlit/vert mesh-attrs))
  (:fragment (unlit-texture-decal/frag :vec4 :vec2)))
