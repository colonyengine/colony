(in-package #:cl-user)

(defpackage #:vorigin.transform3d
  (:local-nicknames
   (#:com #:vorigin.common)
   (#:m2 #:vorigin.mat2)
   (#:m3 #:vorigin.mat3)
   (#:m4 #:vorigin.mat4)
   (#:u #:vutils)
   (#:v3 #:vorigin.vec3)
   (#:v4 #:vorigin.vec4))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  ;; Start of legend First, we describe the actions we use in the categorization:
  ;; everything that says t3 you keep
  (:export
   ;; #:mat
   ;; #:with-components
   ;; #:pretty-print
   ;; #:+zero+
   ;; #:+id+
   ;; #:zero
   ;; #:zero!
   ;; #:zero-p
   ;; #:id
   ;; #:id!
   ;; #:id-p
   ;; #:=
   ;; #:random!
   ;; #:random
   ;; #:copy!
   ;; #:copy
   ;; #:clamp!
   ;; #:clamp
   ;; #:clamp-range!
   ;; #:clamp-range
   ;; #:+!
   ;; #:+
   ;; #:-!
   ;; #:-
   ;; #:*!
   ;; #:*
   ;; #:get-column!
   ;; #:get-column
   ;; #:set-column!
   ;; #:set-column
   ;; #:get-translation!
   ;; #:get-translation
   ;; #:set-translation!
   ;; #:set-translation
   ;; #:translate!
   ;; #:translate
   ;; #:copy-rotation!
   ;; #:copy-rotation
   ;; #:rotation-to-mat3!
   ;; #:rotation-to-mat3
   ;; #:normalize-rotation!
   ;; #:normalize-rotation
   ;; #:rotation-axis-to-vec3!
   ;; #:rotation-axis-to-vec3
   ;; #:rotation-axis-from-vec3!
   ;; #:rotation-axis-from-vec3
   ;; #:rotation-x-from-angle!
   ;; #:rotation-x-from-angle
   ;; #:rotation-y-from-angle!
   ;; #:rotation-y-from-angle!
   ;; #:rotation-z-from-angle
   ;; #:rotation-z-from-angle
   ;; #:rotate!
   ;; #:rotate
   ;; #:get-scale!
   ;; #:get-scale
   ;; #:set-scale!
   ;; #:set-scale
   ;; #:scale!
   ;; #:scale
   ;; #:*v3!
   ;; #:*v3
   ;; #:*v4!
   ;; #:*v4
   ;; #:transpose!
   ;; #:transpose
   ;; #:orthogonal-p
   ;; #:orthonormalize!
   ;; #:orthonormalize
   ;; #:trace
   ;; #:diagonal-p
   ;; #:main-diagonal!
   ;; #:main-diagonal
   ;; #:anti-diagonal!
   ;; #:anti-diagonal
   ;; #:set-diagonal!
   ;; #:set-diagonal
   ;; Do these last. Anything in a defpackage should in alphabetical order.
   ;; #:determinant
   ;; #:invert-orthogonal!
   ;; #:invert-orthogonal
   ;; #:invert!
   ;; #:invert
   #:look-at!
   #:look-at
   ;; These will be in the ticket describing them.
   ;; #:ortho!
   ;; #:ortho
   ;; #:perspective!
   ;; #:perspective
   ))

(in-package #:vorigin.transform3d)

(u:fn-> look-at! (m4:mat v3:vec v3:vec v3:vec) m4:mat)
(defun look-at! (out eye target up)
  (declare (optimize speed))
  (m4:with-components ((o out))
    (let* ((z (v3:- target eye))
           (x (v3:cross z up))
           (y (v3:cross x z)))
      (declare (dynamic-extent x y z))
      (v3:with-components ((x x) (y y) (z z))
        (v3:normalize! x x)
        (v3:normalize! y y)
        (v3:normalize! z z)
        (setf o00 xx
              o01 xy
              o02 xz
              o03 (cl:- (v3:dot x eye))
              o10 yx
              o11 yy
              o12 yz
              o13 (cl:- (v3:dot y eye))
              o20 (cl:- zx)
              o21 (cl:- zy)
              o22 (cl:- zz)
              o23 (v3:dot z eye)
              o30 0f0
              o31 0f0
              o32 0f0
              o33 1f0))))
  out)

(u:fn-> look-at (v3:vec v3:vec v3:vec) m4:mat)
(declaim (inline look-at))
(defun look-at (eye target up)
  (declare (optimize speed))
  ;; TODO: Why didn't the compiler complain about this look-at?
  (look-at! (m4:id) eye target up))
