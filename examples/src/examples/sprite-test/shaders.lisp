(in-package :first-light.gpu.user)

(define-struct sprite-index
  (image :sampler-2d :accessor image)
  (sprite :int32 :accessor sprite))

(define-struct sprite-sheet-data
  (pos (:vec2 2048) :accessor pos)
  (size (:vec2 2048) :accessor size))

(define-function sprite/vert ()
  (values))

(define-function sprite/geom (&uniform
                              (model :mat4)
                              (view :mat4)
                              (proj :mat4)
                              (tex sprite-index)
                              (sprite-sheet sprite-sheet-data :ssbo :std-430))
  (declare (output-primitive :kind :triangle-strip :max-vertices 6))
  (let* ((mvp (* proj view model))
         (extents (vec4 (aref (pos sprite-sheet) (sprite tex))
                        (aref (size sprite-sheet) (sprite tex))))
         (size (.xyxy (texture-size (image tex) 0)))
         (offsets (* size (vec4 (* 0.5 (.zw extents)) (* -0.5 (.zw extents))))))
    (setf (.zw extents) (+ (.xy extents) (.zw extents)))
    (emit ()
          (* mvp (vec4 (.xy offsets) 0 1))
          (.xw extents))
    (emit ()
          (* mvp (vec4 (.zy offsets) 0 1))
          (.zw extents))
    (emit ()
          (* mvp (vec4 (.xw offsets) 0 1))
          (.xy extents))
    (emit ()
          (* mvp (vec4 (.zw offsets) 0 1))
          (.zy extents))
    (end-primitive))
  (values))

(define-function sprite/frag ((uv :vec2)
                              &uniform
                              (tex sprite-index))
  (let ((color (texture (image tex) uv)))
    color))

(define-shader sprite-test (:version 430 :primitive :points)
  (:vertex (sprite/vert))
  (:geometry (sprite/geom))
  (:fragment (sprite/frag :vec2)))
