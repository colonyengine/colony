(in-package :first-light.math)

;;; constants

(fl.util:define-constant +zero-mat4+
    (%mat4 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
  :test #'equalp)

(fl.util:define-constant +id-mat4+
    (%mat4 1f0 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0)
  :test #'equalp)

;;; array accessor

(defspecialization (get-array :inline t) ((matrix mat4)) (simple-array single-float (16))
  (m4-array matrix))


;;; constructors

(defspecialization (mat4 :inline t) () mat4
  (%mat4 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))

(defspecialization (mat4 :inline t) ((x real)) mat4
  (%mat4 (float x 1f0) 0f0 0f0 0f0
         0f0 (float x 1f0) 0f0 0f0
         0f0 0f0 (float x 1f0) 0f0
         0f0 0f0 0f0 (float x 1f0)))

(defspecialization (mat4 :inline t) ((mat mat2)) mat4
  (with-mat2 ((m mat))
    (%mat4 m.00 m.10 0f0 0f0 m.01 m.11 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0)))

(defspecialization (mat4 :inline t) ((mat mat3)) mat4
  (with-mat3 ((m mat))
    (%mat4 m.00 m.10 m.20 0f0 m.01 m.11 m.21 0f0 m.02 m.12 m.22 0f0 0f0 0f0 0f0 1f0)))

(defspecialization (mat4 :inline t) ((mat mat4)) mat4
  (with-mat4 ((m mat))
    (%mat4 m.00 m.10 m.20 m.30 m.01 m.11 m.21 m.31 m.02 m.12 m.22 m.32 m.03 m.13 m.23 m.33)))

(defspecialization (mat4 :inline t) ((quat quat)) mat4
  (with-quat ((q quat))
    (let* ((s (cl:/ 2 (length-squared quat)))
           (xs (cl:* q.x s))
           (ys (cl:* q.y s))
           (zs (cl:* q.z s))
           (xx (cl:* q.x xs))
           (xy (cl:* q.x ys))
           (xz (cl:* q.x zs))
           (yy (cl:* q.y ys))
           (yz (cl:* q.y zs))
           (zz (cl:* q.z zs))
           (wx (cl:* q.w xs))
           (wy (cl:* q.w ys))
           (wz (cl:* q.w zs)))
      (%mat4 (cl:- 1 (cl:+ yy zz)) (cl:+ xy wz) (cl:- xz wy) 0f0
             (cl:- xy wz) (cl:- 1 (cl:+ xx zz)) (cl:+ yz wx) 0f0
             (cl:+ xz wy) (cl:- yz wx) (cl:- 1 (cl:+ xx yy)) 0f0
             0f0 0f0 0f0 1f0))))

(defspecialization (mat4 :inline t) ((quat quat) (out mat4)) mat4
  (with-quat ((q quat))
    (with-mat4 ((o out))
      (let* ((s (cl:/ 2 (length-squared quat)))
             (xs (cl:* q.x s))
             (ys (cl:* q.y s))
             (zs (cl:* q.z s))
             (xx (cl:* q.x xs))
             (xy (cl:* q.x ys))
             (xz (cl:* q.x zs))
             (yy (cl:* q.y ys))
             (yz (cl:* q.y zs))
             (zz (cl:* q.z zs))
             (wx (cl:* q.w xs))
             (wy (cl:* q.w ys))
             (wz (cl:* q.w zs)))
        (setf o.00 (cl:- 1 (cl:+ yy zz))
              o.10 (cl:+ xy wz)
              o.20 (cl:- xz wy)
              o.30 0f0
              o.01 (cl:- xy wz)
              o.11 (cl:- 1 (cl:+ xx zz))
              o.21 (cl:+ yz wx)
              o.31 0f0
              o.02 (cl:+ xz wy)
              o.12 (cl:- yz wx)
              o.22 (cl:- 1 (cl:+ xx yy))
              o.32 0f0
              o.03 0f0
              o.13 0f0
              o.23 0f0
              o.33 1f0))))
  out)

(defspecialization (mat4 :inline t) ((a vec4) (b vec4) (c vec4) (d vec4)) mat4
  (with-vec4 ((a a) (b b) (c c) (d d))
    (%mat4 a.x a.y a.z a.w b.x b.y b.z b.w c.x c.y c.z c.w d.x d.y d.z d.w)))

(defspecialization (mat4 :inline t) ((m00 real) (m10 real) (m20 real) (m30 real)
                                     (m01 real) (m11 real) (m21 real) (m31 real)
                                     (m02 real) (m12 real) (m22 real) (m32 real)
                                     (m03 real) (m13 real) (m23 real) (m33 real))
    mat4
  (%mat4 (float m00 1f0) (float m10 1f0) (float m20 1f0) (float m30 1f0)
         (float m01 1f0) (float m11 1f0) (float m21 1f0) (float m31 1f0)
         (float m02 1f0) (float m12 1f0) (float m22 1f0) (float m32 1f0)
         (float m03 1f0) (float m13 1f0) (float m23 1f0) (float m33 1f0)))

;;; copiers

(defspecialization (copy :inline t) ((in mat4)) mat4
  (with-mat4 ((m in))
    (%mat4 m.00 m.10 m.20 m.30 m.01 m.11 m.21 m.31 m.02 m.12 m.22 m.32 m.03 m.13 m.32 m.33)))

(defspecialization (copy :inline t) ((in mat4) (x real)) mat4
  (with-mat4 ((m in))
    (%mat4 (float x 1f0) m.10 m.20 m.30
           m.01 (float x 1f0) m.21 m.31
           m.02 m.12 (float x 1f0) m.32
           m.03 m.13 m.23 (float x 1f0))))

(defspecialization (copy :inline t) ((in mat4) (x mat2)) mat4
  (with-mat4 ((m in))
    (with-mat2 ((x x))
      (%mat4 x.00 x.10 m.20 m.30 x.01 x.11 m.21 m.31 m.02 m.12 m.22 m.32 m.03 m.13 m.23 m.33))))

(defspecialization (copy :inline t) ((in mat4) (x mat3)) mat4
  (with-mat4 ((m in))
    (with-mat3 ((x x))
      (%mat4 x.00 x.10 x.20 m.30 x.01 x.11 x.21 m.31 x.02 x.12 x.22 m.32 m.03 m.13 m.23 m.33))))

(defspecialization (copy :inline t) ((in mat4) (x mat4)) mat4
  (declare (ignore in))
  (with-mat4 ((x x))
    (%mat4 x.00 x.10 x.20 x.30 x.01 x.11 x.21 x.31 x.02 x.12 x.22 x.32 x.03 x.13 x.23 x.33)))

(defspecialization (copy :inline t) ((in mat4) (a vec4) (b vec4) (c vec4) (d vec4)) mat4
  (declare (ignore in))
  (with-vec4 ((a a) (b b) (c c) (d d))
    (%mat4 a.x a.y a.z a.w b.x b.y b.z b.w c.x c.y c.z c.w d.x d.y d.z d.w)))

(defspecialization (copy :inline t) ((in mat4)
                                     (m00 real) (m10 real) (m20 real) (m30 real)
                                     (m01 real) (m11 real) (m21 real) (m31 real)
                                     (m02 real) (m12 real) (m22 real) (m32 real)
                                     (m03 real) (m13 real) (m23 real) (m33 real))
    mat4
  (declare (ignore in))
  (%mat4 m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33))

(defspecialization (copy-into :inline t) ((out mat4)) mat4
  out)

(defspecialization (copy-into :inline t) ((out mat4) (x real)) mat4
  (with-mat4 ((o out))
    (setf o.00 (float x 1f0) o.11 (float x 1f0) o.22 (float x 1f0) o.33 (float x 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out mat4) (x mat2)) mat4
  (with-mat4 ((o out))
    (with-mat2 ((x x))
      (setf o.00 x.00 o.10 x.10 o.01 x.01 o.11 x.11)))
  out)

(defspecialization (copy-into :inline t) ((out mat4) (x mat3)) mat4
  (with-mat4 ((o out))
    (with-mat3 ((x x))
      (setf o.00 x.00 o.10 x.10 o.20 x.20
            o.01 x.01 o.11 x.11 o.21 x.21
            o.02 x.02 o.12 x.12 o.22 x.22)))
  out)

(defspecialization (copy-into :inline t) ((out mat4) (x mat4)) mat4
  (with-mat4 ((o out) (x x))
    (setf o.00 x.00 o.10 x.10 o.20 x.20 o.30 x.30
          o.01 x.01 o.11 x.11 o.21 x.21 o.31 x.31
          o.02 x.02 o.12 x.12 o.22 x.22 o.32 x.32
          o.03 x.03 o.13 x.13 o.23 x.23 o.33 x.33))
  out)

(defspecialization (copy-into :inline t) ((out mat4) (a vec4) (b vec4) (c vec4) (d vec4)) mat4
  (with-mat4 ((o out))
    (with-vec4 ((a a) (b b) (c c) (d d))
      (setf o.00 a.x o.10 a.y o.20 a.z o.30 a.w
            o.01 b.x o.11 b.y o.21 b.z o.31 b.w
            o.02 c.x o.12 c.y o.22 c.z o.32 c.w
            o.03 d.x o.13 d.y o.23 d.z o.33 d.w)))
  out)

(defspecialization (copy-into :inline t) ((out mat4)
                                          (m00 real) (m10 real) (m20 real) (m30 real)
                                          (m01 real) (m11 real) (m21 real) (m31 real)
                                          (m02 real) (m12 real) (m22 real) (m32 real)
                                          (m03 real) (m13 real) (m23 real) (m33 real))
    mat4
  (with-mat4 ((o out))
    (setf o.00 (float m00 1f0) o.10 (float m10 1f0) o.20 (float m20 1f0) o.30 (float m30 1f0)
          o.01 (float m01 1f0) o.11 (float m11 1f0) o.21 (float m21 1f0) o.31 (float m31 1f0)
          o.02 (float m02 1f0) o.12 (float m12 1f0) o.22 (float m22 1f0) o.32 (float m32 1f0)
          o.03 (float m03 1f0) o.13 (float m13 1f0) o.23 (float m23 1f0) o.33 (float m33 1f0)))
  out)

;;; operations

(defspecialization (rand :inline t) ((in mat4) (min real) (max real) (out mat4)) mat4
  (declare (ignore in))
  (let ((min (float min 1f0))
        (max (float max 1f0)))
    (with-mat4 ((o out))
      (psetf o.00 (cl:+ min (random (cl:- max min)))
             o.01 (cl:+ min (random (cl:- max min)))
             o.02 (cl:+ min (random (cl:- max min)))
             o.03 (cl:+ min (random (cl:- max min)))
             o.10 (cl:+ min (random (cl:- max min)))
             o.11 (cl:+ min (random (cl:- max min)))
             o.12 (cl:+ min (random (cl:- max min)))
             o.13 (cl:+ min (random (cl:- max min)))
             o.20 (cl:+ min (random (cl:- max min)))
             o.21 (cl:+ min (random (cl:- max min)))
             o.22 (cl:+ min (random (cl:- max min)))
             o.23 (cl:+ min (random (cl:- max min)))
             o.30 (cl:+ min (random (cl:- max min)))
             o.31 (cl:+ min (random (cl:- max min)))
             o.32 (cl:+ min (random (cl:- max min)))
             o.33 (cl:+ min (random (cl:- max min))))))
  out)

(defspecialization (rand :inline t) ((in mat4) (min real) (max real) (out null)) mat4
  (declare (ignore out))
  (rand in min max (the mat4 (mat4))))

(defspecialization (zero :inline t) ((out mat4)) mat4
  (with-mat4 ((m out))
    (psetf m.00 0f0 m.10 0f0 m.20 0f0 m.30 0f0
           m.01 0f0 m.11 0f0 m.21 0f0 m.31 0f0
           m.02 0f0 m.12 0f0 m.22 0f0 m.32 0f0
           m.03 0f0 m.13 0f0 m.23 0f0 m.33 0f0))
  out)

(defspecialization (zero-p :inline t) ((in mat4)) boolean
  (with-mat4 ((m in))
    (cl:= 0f0 m.00 m.10 m.20 m.30 m.01 m.11 m.21 m.31 m.02 m.12 m.22 m.32 m.03 m.13 m.23 m.33)))

(defspecialization (id :inline t) ((out mat4)) mat4
  (with-mat4 ((m out))
    (psetf m.00 1f0 m.10 0f0 m.20 0f0 m.30 0f0
           m.01 0f0 m.11 1f0 m.21 0f0 m.31 0f0
           m.02 0f0 m.12 0f0 m.22 1f0 m.32 0f0
           m.03 0f0 m.13 0f0 m.23 0f0 m.33 1f0))
  out)

(defspecialization (id-p :inline t) ((in mat4)) boolean
  (with-mat4 ((m in))
    (and (cl:= 0f0 m.10 m.20 m.30 m.01 m.21 m.31 m.02 m.12 m.32 m.03 m.13 m.23)
         (cl:= 1f0 m.00 m.11 m.22 m.33))))

(defspecialization (clamp :inline t) ((in mat4) (min real) (max real) (out mat4)) mat4
  (with-mat4 ((m in) (o out))
    (psetf o.00 (float (fl.util:clamp m.00 min max) 1f0)
           o.01 (float (fl.util:clamp m.01 min max) 1f0)
           o.02 (float (fl.util:clamp m.02 min max) 1f0)
           o.03 (float (fl.util:clamp m.03 min max) 1f0)
           o.10 (float (fl.util:clamp m.10 min max) 1f0)
           o.11 (float (fl.util:clamp m.11 min max) 1f0)
           o.12 (float (fl.util:clamp m.12 min max) 1f0)
           o.13 (float (fl.util:clamp m.13 min max) 1f0)
           o.20 (float (fl.util:clamp m.20 min max) 1f0)
           o.21 (float (fl.util:clamp m.21 min max) 1f0)
           o.22 (float (fl.util:clamp m.22 min max) 1f0)
           o.23 (float (fl.util:clamp m.23 min max) 1f0)
           o.30 (float (fl.util:clamp m.30 min max) 1f0)
           o.31 (float (fl.util:clamp m.31 min max) 1f0)
           o.32 (float (fl.util:clamp m.32 min max) 1f0)
           o.33 (float (fl.util:clamp m.33 min max) 1f0)))
  out)

(defspecialization (clamp :inline t) ((in mat4) (min real) (max real) (out null)) mat4
  (declare (ignore out))
  (clamp in min max (the mat4 (mat4))))

(defspecialization (stabilize :inline t) ((in mat4) (tolerance single-float) (out mat4)) mat4
  (with-mat4 ((m in) (o out))
    (macrolet ((%stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0f0 ,place)))
      (psetf o.00 (%stabilize m.00)
             o.01 (%stabilize m.01)
             o.02 (%stabilize m.02)
             o.03 (%stabilize m.03)
             o.10 (%stabilize m.10)
             o.11 (%stabilize m.11)
             o.12 (%stabilize m.12)
             o.13 (%stabilize m.13)
             o.20 (%stabilize m.20)
             o.21 (%stabilize m.21)
             o.22 (%stabilize m.22)
             o.23 (%stabilize m.23)
             o.30 (%stabilize m.30)
             o.31 (%stabilize m.31)
             o.32 (%stabilize m.32)
             o.33 (%stabilize m.33))))
  out)

(defspecialization (stabilize :inline t) ((in mat4) (tolerance single-float) (out null)) mat4
  (declare (ignore out))
  (stabilize in tolerance (the mat4 (mat4))))

(defspecialization (+ :inline t) ((in1 mat4) (in2 mat4) (out mat4)) mat4
  (with-mat4 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:+ m1.00 m2.00)
           o.01 (cl:+ m1.01 m2.01)
           o.02 (cl:+ m1.02 m2.02)
           o.03 (cl:+ m1.03 m2.03)
           o.10 (cl:+ m1.10 m2.10)
           o.11 (cl:+ m1.11 m2.11)
           o.12 (cl:+ m1.12 m2.12)
           o.13 (cl:+ m1.13 m2.13)
           o.20 (cl:+ m1.20 m2.20)
           o.21 (cl:+ m1.21 m2.21)
           o.22 (cl:+ m1.22 m2.22)
           o.23 (cl:+ m1.23 m2.23)
           o.30 (cl:+ m1.30 m2.30)
           o.31 (cl:+ m1.31 m2.31)
           o.32 (cl:+ m1.32 m2.32)
           o.33 (cl:+ m1.33 m2.33)))
  out)

(defspecialization (+ :inline t) ((in1 mat4) (in2 mat4) (out null)) mat4
  (declare (ignore out))
  (+ in1 in2 (the mat4 (mat4))))

(defspecialization (- :inline t) ((in1 mat4) (in2 mat4) (out mat4)) mat4
  (with-mat4 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:- m1.00 m2.00)
           o.01 (cl:- m1.01 m2.01)
           o.02 (cl:- m1.02 m2.02)
           o.03 (cl:- m1.03 m2.03)
           o.10 (cl:- m1.10 m2.10)
           o.11 (cl:- m1.11 m2.11)
           o.12 (cl:- m1.12 m2.12)
           o.13 (cl:- m1.13 m2.13)
           o.20 (cl:- m1.20 m2.20)
           o.21 (cl:- m1.21 m2.21)
           o.22 (cl:- m1.22 m2.22)
           o.23 (cl:- m1.23 m2.23)
           o.30 (cl:- m1.30 m2.30)
           o.31 (cl:- m1.31 m2.31)
           o.32 (cl:- m1.32 m2.32)
           o.33 (cl:- m1.33 m2.33)))
  out)

(defspecialization (- :inline t) ((in1 mat4) (in2 mat4) (out null)) mat4
  (declare (ignore out))
  (- in1 in2 (the mat4 (mat4))))

(defspecialization (* :inline t) ((in1 mat4) (in2 mat4) (out mat4)) mat4
  (with-mat4 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:+ (cl:* m1.00 m2.00) (cl:* m1.01 m2.10) (cl:* m1.02 m2.20) (cl:* m1.03 m2.30))
           o.10 (cl:+ (cl:* m1.10 m2.00) (cl:* m1.11 m2.10) (cl:* m1.12 m2.20) (cl:* m1.13 m2.30))
           o.20 (cl:+ (cl:* m1.20 m2.00) (cl:* m1.21 m2.10) (cl:* m1.22 m2.20) (cl:* m1.23 m2.30))
           o.30 (cl:+ (cl:* m1.30 m2.00) (cl:* m1.31 m2.10) (cl:* m1.32 m2.20) (cl:* m1.33 m2.30))
           o.01 (cl:+ (cl:* m1.00 m2.01) (cl:* m1.01 m2.11) (cl:* m1.02 m2.21) (cl:* m1.03 m2.31))
           o.11 (cl:+ (cl:* m1.10 m2.01) (cl:* m1.11 m2.11) (cl:* m1.12 m2.21) (cl:* m1.13 m2.31))
           o.21 (cl:+ (cl:* m1.20 m2.01) (cl:* m1.21 m2.11) (cl:* m1.22 m2.21) (cl:* m1.23 m2.31))
           o.31 (cl:+ (cl:* m1.30 m2.01) (cl:* m1.31 m2.11) (cl:* m1.32 m2.21) (cl:* m1.33 m2.31))
           o.02 (cl:+ (cl:* m1.00 m2.02) (cl:* m1.01 m2.12) (cl:* m1.02 m2.22) (cl:* m1.03 m2.32))
           o.12 (cl:+ (cl:* m1.10 m2.02) (cl:* m1.11 m2.12) (cl:* m1.12 m2.22) (cl:* m1.13 m2.32))
           o.22 (cl:+ (cl:* m1.20 m2.02) (cl:* m1.21 m2.12) (cl:* m1.22 m2.22) (cl:* m1.23 m2.32))
           o.32 (cl:+ (cl:* m1.30 m2.02) (cl:* m1.31 m2.12) (cl:* m1.32 m2.22) (cl:* m1.33 m2.32))
           o.03 (cl:+ (cl:* m1.00 m2.03) (cl:* m1.01 m2.13) (cl:* m1.02 m2.23) (cl:* m1.03 m2.33))
           o.13 (cl:+ (cl:* m1.10 m2.03) (cl:* m1.11 m2.13) (cl:* m1.12 m2.23) (cl:* m1.13 m2.33))
           o.23 (cl:+ (cl:* m1.20 m2.03) (cl:* m1.21 m2.13) (cl:* m1.22 m2.23) (cl:* m1.23 m2.33))
           o.33 (cl:+ (cl:* m1.30 m2.03) (cl:* m1.31 m2.13) (cl:* m1.32 m2.23) (cl:* m1.33 m2.33))))
  out)

(defspecialization (* :inline t) ((in1 mat4) (in2 mat4) (out null)) mat4
  (declare (ignore out))
  (* in1 in2 (the mat4 (mat4))))

(defspecialization (* :inline t) ((in1 mat4) (in2 vec4) (out vec4)) vec4
  (with-mat4 ((m in1))
    (with-vec4 ((v in2) (o out))
      (psetf o.x (cl:+ (cl:* m.00 v.x) (cl:* m.01 v.y) (cl:* m.02 v.z) (cl:* m.03 v.w))
             o.y (cl:+ (cl:* m.10 v.x) (cl:* m.11 v.y) (cl:* m.12 v.z) (cl:* m.13 v.w))
             o.z (cl:+ (cl:* m.20 v.x) (cl:* m.21 v.y) (cl:* m.22 v.z) (cl:* m.23 v.w))
             o.w (cl:+ (cl:* m.30 v.x) (cl:* m.31 v.y) (cl:* m.32 v.z) (cl:* m.33 v.w)))))
  out)

(defspecialization (* :inline t) ((in1 mat4) (in2 vec4) (out null)) vec4
  (declare (ignore out))
  (* in1 in2 (the vec4 (vec4))))

(defspecialization (* :inline t) ((in1 mat4) (in2 real) (out mat4)) mat4
  (with-mat4 ((m in1) (o out))
    (psetf o.00 (cl:* m.00 in2)
           o.01 (cl:* m.01 in2)
           o.02 (cl:* m.02 in2)
           o.03 (cl:* m.03 in2)
           o.10 (cl:* m.10 in2)
           o.11 (cl:* m.11 in2)
           o.12 (cl:* m.12 in2)
           o.13 (cl:* m.13 in2)
           o.20 (cl:* m.20 in2)
           o.21 (cl:* m.21 in2)
           o.22 (cl:* m.22 in2)
           o.23 (cl:* m.23 in2)
           o.30 (cl:* m.30 in2)
           o.31 (cl:* m.31 in2)
           o.32 (cl:* m.32 in2)
           o.33 (cl:* m.33 in2)))
  out)

(defspecialization (* :inline t) ((in1 mat4) (in2 real) (out null)) mat4
  (declare (ignore out))
  (* in1 in2 (the mat4 (mat4))))

(defspecialization (/ :inline t) ((in1 mat4) (in2 real) (out mat4)) mat4
  (with-mat4 ((m in1) (o out))
    (psetf o.00 (if (zerop in2) 0f0 (cl:/ m.00 in2))
           o.01 (if (zerop in2) 0f0 (cl:/ m.01 in2))
           o.02 (if (zerop in2) 0f0 (cl:/ m.02 in2))
           o.03 (if (zerop in2) 0f0 (cl:/ m.03 in2))
           o.10 (if (zerop in2) 0f0 (cl:/ m.10 in2))
           o.11 (if (zerop in2) 0f0 (cl:/ m.11 in2))
           o.12 (if (zerop in2) 0f0 (cl:/ m.12 in2))
           o.13 (if (zerop in2) 0f0 (cl:/ m.13 in2))
           o.20 (if (zerop in2) 0f0 (cl:/ m.20 in2))
           o.21 (if (zerop in2) 0f0 (cl:/ m.21 in2))
           o.22 (if (zerop in2) 0f0 (cl:/ m.22 in2))
           o.23 (if (zerop in2) 0f0 (cl:/ m.23 in2))
           o.30 (if (zerop in2) 0f0 (cl:/ m.30 in2))
           o.31 (if (zerop in2) 0f0 (cl:/ m.31 in2))
           o.32 (if (zerop in2) 0f0 (cl:/ m.32 in2))
           o.33 (if (zerop in2) 0f0 (cl:/ m.33 in2))))
  out)

(defspecialization (/ :inline t) ((in1 mat4) (in2 real) (out null)) mat4
  (declare (ignore out))
  (/ in1 in2 (the mat4 (mat4))))

(defspecialization (= :inline t) ((in1 mat4) (in2 mat4)) boolean
  (with-mat4 ((m1 in1) (m2 in2))
    (and (cl:= m1.00 m2.00) (cl:= m1.01 m2.01) (cl:= m1.02 m2.02) (cl:= m1.03 m2.03)
         (cl:= m1.10 m2.10) (cl:= m1.11 m2.11) (cl:= m1.12 m2.12) (cl:= m1.13 m2.13)
         (cl:= m1.20 m2.20) (cl:= m1.21 m2.21) (cl:= m1.22 m2.22) (cl:= m1.23 m2.23)
         (cl:= m1.30 m2.30) (cl:= m1.31 m2.31) (cl:= m1.32 m2.32) (cl:= m1.33 m2.33))))

(defspecialization (~ :inline t) ((in1 mat4) (in2 mat4)) boolean
  (with-mat4 ((m1 in1) (m2 in2))
    (and (cl:< (cl:abs (cl:- m1.00 m2.00)) 1e-7)
         (cl:< (cl:abs (cl:- m1.01 m2.01)) 1e-7)
         (cl:< (cl:abs (cl:- m1.02 m2.02)) 1e-7)
         (cl:< (cl:abs (cl:- m1.03 m2.03)) 1e-7)
         (cl:< (cl:abs (cl:- m1.10 m2.10)) 1e-7)
         (cl:< (cl:abs (cl:- m1.11 m2.11)) 1e-7)
         (cl:< (cl:abs (cl:- m1.12 m2.12)) 1e-7)
         (cl:< (cl:abs (cl:- m1.13 m2.13)) 1e-7)
         (cl:< (cl:abs (cl:- m1.20 m2.20)) 1e-7)
         (cl:< (cl:abs (cl:- m1.21 m2.21)) 1e-7)
         (cl:< (cl:abs (cl:- m1.22 m2.22)) 1e-7)
         (cl:< (cl:abs (cl:- m1.23 m2.23)) 1e-7)
         (cl:< (cl:abs (cl:- m1.30 m2.31)) 1e-7)
         (cl:< (cl:abs (cl:- m1.31 m2.32)) 1e-7)
         (cl:< (cl:abs (cl:- m1.32 m2.32)) 1e-7)
         (cl:< (cl:abs (cl:- m1.33 m2.33)) 1e-7))))

(defspecialization (get-column :inline t) ((mat mat4) (index (integer 0 3)) (out vec4)) vec4
  (with-mat4 ((m mat))
    (with-vec4 ((o out))
      (ecase index
        (0 (setf o.x m.00 o.y m.10 o.z m.20 o.w m.30))
        (1 (setf o.x m.01 o.y m.11 o.z m.21 o.w m.31))
        (2 (setf o.x m.02 o.y m.12 o.z m.22 o.w m.32))
        (3 (setf o.x m.03 o.y m.13 o.z m.23 o.w m.33)))))
  out)

(defspecialization (get-column :inline t) ((mat mat4) (index (integer 0 3)) (out null)) vec4
  (declare (ignore out))
  (get-column mat index (the vec4 (vec4))))

(defspecialization (set-column :inline t) ((mat mat4) (index (integer 0 3)) (vec vec3) (out mat4))
    mat4
  (with-mat4 ((o out))
    (with-vec3 ((v vec))
      (copy-into out mat)
      (ecase index
        (0 (setf o.00 v.x o.10 v.y o.20 v.z))
        (1 (setf o.01 v.x o.11 v.y o.21 v.z))
        (2 (setf o.02 v.x o.12 v.y o.22 v.z))
        (3 (setf o.03 v.x o.13 v.y o.23 v.z)))))
  out)

(defspecialization (set-column :inline t) ((mat mat4) (index (integer 0 3)) (vec vec3) (out null))
    mat4
  (declare (ignore out))
  (set-column mat index vec (the mat4 (mat4))))

(defspecialization (set-column :inline t) ((mat mat4) (index (integer 0 3)) (vec vec4) (out mat4))
    mat4
  (with-mat4 ((o out))
    (with-vec4 ((v vec))
      (copy-into out mat)
      (ecase index
        (0 (setf o.00 v.x o.10 v.y o.20 v.z o.30 v.w))
        (1 (setf o.01 v.x o.11 v.y o.21 v.z o.31 v.w))
        (2 (setf o.02 v.x o.12 v.y o.22 v.z o.32 v.w))
        (3 (setf o.03 v.x o.13 v.y o.23 v.z o.33 v.w)))))
  out)

(defspecialization (set-column :inline t) ((mat mat4) (index (integer 0 3)) (vec vec4) (out null))
    mat4
  (declare (ignore out))
  (set-column mat index vec (the mat4 (mat4))))

(defspecialization (get-translation :inline t) ((mat mat4) (out vec3)) vec3
  (with-mat4 ((m mat))
    (with-vec3 ((o out))
      (psetf o.x m.03 o.y m.13 o.z m.23)))
  out)

(defspecialization (get-translation :inline t) ((mat mat4) (out null)) vec3
  (declare (ignore out))
  (get-translation mat (the vec3 (vec3))))

(defspecialization (set-translation :inline t) ((mat mat4) (vec vec3) (out mat4)) mat4
  (with-mat4 ((o out))
    (with-vec3 ((v vec))
      (copy-into out mat)
      (psetf o.03 v.x o.13 v.y o.23 v.z)))
  out)

(defspecialization (set-translation :inline t) ((mat mat4) (vec vec3) (out null)) mat4
  (declare (ignore out))
  (set-translation mat vec (the mat4 (copy mat))))

(defspecialization (translate :inline t) ((mat mat4) (vec vec3) (out mat4)) mat4
  (* (the mat4 (set-translation (mat4 1) vec)) mat out))

(defspecialization (translate :inline t) ((mat mat4) (vec vec3) (out null)) mat4
  (declare (ignore out))
  (translate mat vec (the mat4 (mat4 1))))

(defspecialization (rotate :inline t) ((kind (eql :local)) (in mat4) (angle vec3) (out mat4)) mat4
  (declare (ignore kind))
  (macrolet ((rotate-angle (angle s c &body body)
               `(when (cl:> (cl:abs ,angle) 1e-7)
                  (let ((,s (cl:sin ,angle))
                        (,c (cl:cos ,angle)))
                    ,@body
                    (* out m out)))))
    (with-mat4 ((m (mat4 1)))
      (with-vec3 ((v angle))
        (copy-into out in)
        (rotate-angle v.z s c
                      (psetf m.00 c m.01 (cl:- s)
                             m.10 s m.11 c))
        (rotate-angle v.x s c
                      (psetf m.00 1f0 m.01 0f0 m.02 0f0
                             m.10 0f0 m.11 c m.12 (cl:- s)
                             m.20 0f0 m.21 s m.22 c))
        (rotate-angle v.y s c
                      (psetf m.00 c m.01 0f0 m.02 s
                             m.10 0f0 m.11 1f0 m.12 0f0
                             m.20 (cl:- s) m.21 0f0 m.22 c)))))
  out)

(defspecialization (rotate :inline t) ((kind (eql :local)) (in mat4) (angle vec3) (out null)) mat4
  (declare (ignore out))
  (rotate kind in angle (the mat4 (mat4 1))))

(defspecialization (get-scale :inline t) ((mat mat4) (out vec3)) vec3
  (with-mat4 ((m mat))
    (with-vec3 ((o out))
      (psetf o.x m.00 o.y m.11 o.z m.22)))
  out)

(defspecialization (get-scale :inline t) ((mat mat4) (out null)) vec3
  (declare (ignore out))
  (get-scale mat (the vec3 (vec3))))

(defspecialization (set-scale :inline t) ((mat mat4) (vec vec3) (out mat4)) mat4
  (with-mat4 ((o out))
    (with-vec3 ((v vec))
      (copy-into out mat)
      (psetf o.00 v.x o.11 v.y o.22 v.z)))
  out)

(defspecialization (set-scale :inline t) ((mat mat4) (vec vec3) (out null)) mat4
  (declare (ignore out))
  (set-scale mat vec (the mat4 (copy mat))))

(defspecialization (scale :inline t) ((mat mat4) (vec vec3) (out mat4)) mat4
  (* (the mat4 (set-scale (mat4 1) vec)) mat out))

(defspecialization (scale :inline t) ((mat mat4) (vec vec3) (out null)) mat4
  (declare (ignore out))
  (scale mat vec (the mat4 (mat4 1))))

(defspecialization (transpose :inline t) ((mat mat4) (out mat4)) mat4
  (with-mat4 ((o (copy-into out mat)))
    (rotatef o.01 o.10)
    (rotatef o.02 o.20)
    (rotatef o.03 o.30)
    (rotatef o.12 o.21)
    (rotatef o.13 o.31)
    (rotatef o.23 o.32))
  out)

(defspecialization (transpose :inline t) ((mat mat4) (out null)) mat4
  (declare (ignore out))
  (transpose mat (the mat4 (mat4 1))))

(defspecialization (orthogonal-p :inline t) ((mat mat4)) boolean
  (~ (the mat4 (* mat (transpose mat))) (the mat4 (mat4 1))))

(defspecialization (orthonormalize :inline t) ((mat mat4) (out mat4)) mat4
  (let ((x (vec3 (get-column mat 0)))
        (y (vec3 (get-column mat 1)))
        (z (vec3 (get-column mat 2))))
    (normalize x x)
    (normalize (- y (* x (dot y x))) y)
    (cross x y z)
    (set-column out 0 x out)
    (set-column out 1 y out)
    (set-column out 2 z out))
  out)

(defspecialization (orthonormalize :inline t) ((mat mat4) (out null)) mat4
  (declare (ignore out))
  (orthonormalize mat (the mat4 (mat4 1))))

(defspecialization (trace :inline t) ((mat mat4)) single-float
  (with-mat4 ((m mat))
    (cl:+ m.00 m.11 m.22 m.33)))

(defspecialization (diagonal-p :inline t) ((mat mat4)) boolean
  (with-mat4 ((m mat))
    (cl:= 0f0 m.01 m.02 m.03 m.10 m.12 m.13 m.20 m.21 m.23 m.30 m.31 m.32)))

(defspecialization (main-diagonal :inline t) ((mat mat4) (out vec4)) vec4
  (with-mat4 ((m mat))
    (with-vec4 ((o out))
      (setf o.x m.00 o.y m.11 o.z m.22 o.w m.33)))
  out)

(defspecialization (main-diagonal :inline t) ((mat mat4) (out null)) vec4
  (declare (ignore out))
  (main-diagonal mat (the vec4 (vec4))))

(defspecialization (anti-diagonal :inline t) ((mat mat4) (out vec4)) vec4
  (with-mat4 ((m mat))
    (with-vec4 ((o out))
      (setf o.x m.03 o.y m.12 o.z m.21 o.w m.30)))
  out)

(defspecialization (anti-diagonal :inline t) ((mat mat4) (out null)) vec4
  (declare (ignore out))
  (anti-diagonal mat (the vec4 (vec4))))

(defspecialization (determinant :inline t) ((mat mat4)) single-float
  (with-mat4 ((m mat))
    (cl:- (cl:+ (cl:* m.00 m.11 m.22 m.33) (cl:* m.00 m.12 m.23 m.31) (cl:* m.00 m.13 m.21 m.32)
                (cl:* m.01 m.10 m.23 m.32) (cl:* m.01 m.12 m.20 m.33) (cl:* m.01 m.13 m.22 m.30)
                (cl:* m.02 m.10 m.21 m.33) (cl:* m.02 m.11 m.23 m.30) (cl:* m.02 m.13 m.20 m.31)
                (cl:* m.03 m.10 m.22 m.31) (cl:* m.03 m.11 m.20 m.32) (cl:* m.03 m.12 m.21 m.30))
          (cl:* m.00 m.11 m.23 m.32) (cl:* m.00 m.12 m.21 m.33) (cl:* m.00 m.13 m.22 m.31)
          (cl:* m.01 m.10 m.22 m.33) (cl:* m.01 m.12 m.23 m.30) (cl:* m.01 m.13 m.20 m.32)
          (cl:* m.02 m.10 m.23 m.31) (cl:* m.02 m.11 m.20 m.33) (cl:* m.02 m.13 m.21 m.30)
          (cl:* m.03 m.10 m.21 m.32) (cl:* m.03 m.11 m.22 m.30) (cl:* m.03 m.12 m.20 m.31))))

(defspecialization (invert-orthonormal :inline t) ((mat mat4) (out mat4)) mat4
  (copy-into out mat)
  (with-mat4 ((o out))
    (rotatef o.10 o.01)
    (rotatef o.20 o.02)
    (rotatef o.21 o.12)
    (psetf o.03 (cl:+ (cl:* o.00 (cl:- o.03)) (cl:* o.01 (cl:- o.13)) (cl:* o.02 (cl:- o.23)))
           o.13 (cl:+ (cl:* o.10 (cl:- o.03)) (cl:* o.11 (cl:- o.13)) (cl:* o.12 (cl:- o.23)))
           o.23 (cl:+ (cl:* o.20 (cl:- o.03)) (cl:* o.21 (cl:- o.13)) (cl:* o.22 (cl:- o.23)))))
  out)

(defspecialization (invert-orthonormal :inline t) ((mat mat4) (out null)) mat4
  (declare (ignore out))
  (invert-orthonormal mat (the mat4 (mat4 1))))

(defspecialization (invert :inline t) ((mat mat4) (out mat4)) mat4
  (let ((determinant (determinant mat)))
    (with-mat4 ((m mat) (o out))
      (psetf o.00 (cl:/ (cl:- (cl:+ (cl:* m.11 m.22 m.33) (cl:* m.12 m.23 m.31)
                                    (cl:* m.13 m.21 m.32))
                              (cl:* m.11 m.23 m.32) (cl:* m.12 m.21 m.33) (cl:* m.13 m.22 m.31))
                        determinant)
             o.01 (cl:/ (cl:- (cl:+ (cl:* m.01 m.23 m.32) (cl:* m.02 m.21 m.33)
                                    (cl:* m.03 m.22 m.31))
                              (cl:* m.01 m.22 m.33) (cl:* m.02 m.23 m.31) (cl:* m.03 m.21 m.32))
                        determinant)
             o.02 (cl:/ (cl:- (cl:+ (cl:* m.01 m.12 m.33) (cl:* m.02 m.13 m.31)
                                    (cl:* m.03 m.11 m.32))
                              (cl:* m.01 m.13 m.32) (cl:* m.02 m.11 m.33) (cl:* m.03 m.12 m.31))
                        determinant)
             o.03 (cl:/ (cl:- (cl:+ (cl:* m.01 m.13 m.22) (cl:* m.02 m.11 m.23)
                                    (cl:* m.03 m.12 m.21))
                              (cl:* m.01 m.12 m.23) (cl:* m.02 m.13 m.21) (cl:* m.03 m.11 m.22))
                        determinant)
             o.10 (cl:/ (cl:- (cl:+ (cl:* m.10 m.23 m.32) (cl:* m.12 m.20 m.33)
                                    (cl:* m.13 m.22 m.30))
                              (cl:* m.10 m.22 m.33) (cl:* m.12 m.23 m.30) (cl:* m.13 m.20 m.32))
                        determinant)
             o.11 (cl:/ (cl:- (cl:+ (cl:* m.00 m.22 m.33) (cl:* m.02 m.23 m.30)
                                    (cl:* m.03 m.20 m.32))
                              (cl:* m.00 m.23 m.32) (cl:* m.02 m.20 m.33) (cl:* m.03 m.22 m.30))
                        determinant)
             o.12 (cl:/ (cl:- (cl:+ (cl:* m.00 m.13 m.32) (cl:* m.02 m.10 m.33)
                                    (cl:* m.03 m.12 m.30))
                              (cl:* m.00 m.12 m.33) (cl:* m.02 m.13 m.30) (cl:* m.03 m.10 m.32))
                        determinant)
             o.13 (cl:/ (cl:- (cl:+ (cl:* m.00 m.12 m.23) (cl:* m.02 m.13 m.20)
                                    (cl:* m.03 m.10 m.22))
                              (cl:* m.00 m.13 m.22) (cl:* m.02 m.10 m.23) (cl:* m.03 m.12 m.20))
                        determinant)
             o.20 (cl:/ (cl:- (cl:+ (cl:* m.10 m.21 m.33) (cl:* m.11 m.23 m.30)
                                    (cl:* m.13 m.20 m.31))
                              (cl:* m.10 m.23 m.31) (cl:* m.11 m.20 m.33) (cl:* m.13 m.21 m.30))
                        determinant)
             o.21 (cl:/ (cl:- (cl:+ (cl:* m.00 m.23 m.31) (cl:* m.01 m.20 m.33)
                                    (cl:* m.03 m.21 m.30))
                              (cl:* m.00 m.21 m.33) (cl:* m.01 m.23 m.30) (cl:* m.03 m.20 m.31))
                        determinant)
             o.22 (cl:/ (cl:- (cl:+ (cl:* m.00 m.11 m.33) (cl:* m.01 m.13 m.30)
                                    (cl:* m.03 m.10 m.31))
                              (cl:* m.00 m.13 m.31) (cl:* m.01 m.10 m.33) (cl:* m.03 m.11 m.30))
                        determinant)
             o.23 (cl:/ (cl:- (cl:+ (cl:* m.00 m.13 m.21) (cl:* m.01 m.10 m.23)
                                    (cl:* m.03 m.11 m.20))
                              (cl:* m.00 m.11 m.23) (cl:* m.01 m.13 m.20) (cl:* m.03 m.10 m.21))
                        determinant)
             o.30 (cl:/ (cl:- (cl:+ (cl:* m.10 m.22 m.31) (cl:* m.11 m.20 m.32)
                                    (cl:* m.12 m.21 m.30))
                              (cl:* m.10 m.21 m.32) (cl:* m.11 m.22 m.30) (cl:* m.12 m.20 m.31))
                        determinant)
             o.31 (cl:/ (cl:- (cl:+ (cl:* m.00 m.21 m.32) (cl:* m.01 m.22 m.30)
                                    (cl:* m.02 m.20 m.31))
                              (cl:* m.00 m.22 m.31) (cl:* m.01 m.20 m.32) (cl:* m.02 m.21 m.30))
                        determinant)
             o.32 (cl:/ (cl:- (cl:+ (cl:* m.00 m.12 m.31) (cl:* m.01 m.10 m.32)
                                    (cl:* m.02 m.11 m.30))
                              (cl:* m.00 m.11 m.32) (cl:* m.01 m.12 m.30) (cl:* m.02 m.10 m.31))
                        determinant)
             o.33 (cl:/ (cl:- (cl:+ (cl:* m.00 m.11 m.22) (cl:* m.01 m.12 m.20)
                                    (cl:* m.02 m.10 m.21))
                              (cl:* m.00 m.12 m.21) (cl:* m.01 m.10 m.22) (cl:* m.02 m.11 m.20))
                        determinant))))
  out)

(defspecialization (invert :inline t) ((mat mat4) (out null)) mat4
  (declare (ignore out))
  (invert mat (the mat4 (mat4 1))))

(defspecialization (set-view :inline t) ((eye vec3) (target vec3) (up vec3) (out mat4)) mat4
  ;; based on http://pages.cs.wisc.edu/~psilord/view.txt
  (with-mat4 ((o (id out)))
    (with-vec3 ((e eye) (s target) (u up))
      (macrolet ((%normalize (place-x x place-y y place-z z)
                   (fl.util:once-only (x y z)
                     `(let ((denom (cl:sqrt (cl:+ (cl:* ,x ,x) (cl:* ,y ,y) (cl:* ,z ,z)))))
                        (psetf ,place-x (cl:/ ,x denom)
                               ,place-y (cl:/ ,y denom)
                               ,place-z (cl:/ ,z denom))))))
        (%normalize o.20 (cl:- s.x e.x)
                    o.21 (cl:- s.y e.y)
                    o.22 (cl:- s.z e.z))
        (%normalize o.00 (cl:- (cl:* o.21 u.z) (cl:* o.22 u.y))
                    o.01 (cl:- (cl:* o.22 u.x) (cl:* o.20 u.z))
                    o.02 (cl:- (cl:* o.20 u.y) (cl:* o.21 u.x)))
        (psetf o.10 (cl:- (cl:* o.01 o.22) (cl:* o.02 o.21))
               o.11 (cl:- (cl:* o.02 o.20) (cl:* o.00 o.22))
               o.12 (cl:- (cl:* o.00 o.21) (cl:* o.01 o.20)))
        (psetf o.20 (cl:- o.20)
               o.21 (cl:- o.21)
               o.22 (cl:- o.22))
        (psetf o.03 (cl:+ (cl:* o.00 (cl:- e.x))
                          (cl:* o.01 (cl:- e.y))
                          (cl:* o.02 (cl:- e.z))
                          o.03)
               o.13 (cl:+ (cl:* o.10 (cl:- e.x))
                          (cl:* o.11 (cl:- e.y))
                          (cl:* o.12 (cl:- e.z))
                          o.13)
               o.23 (cl:+ (cl:* o.20 (cl:- e.x))
                          (cl:* o.21 (cl:- e.y))
                          (cl:* o.22 (cl:- e.z))
                          o.23)
               o.33 (cl:+ (cl:* o.30 (cl:- e.x))
                          (cl:* o.31 (cl:- e.y))
                          (cl:* o.32 (cl:- e.z))
                          o.33)))))
  out)

(defspecialization (set-view :inline t) ((eye vec3) (target vec3) (up vec3) (out null)) mat4
  (declare (ignore out))
  (set-view eye target up (the mat4 (mat4 1))))

(defspecialization (set-projection/orthographic :inline t) ((left real) (right real) (bottom real)
                                                            (top real) (near real) (far real)
                                                            (out mat4))
    mat4
  (let ((right-left (float (cl:- right left) 1f0))
        (top-bottom (float (cl:- top bottom) 1f0))
        (far-near (float (cl:- far near) 1f0)))
    (with-mat4 ((m (id out)))
      (psetf m.00 (cl:/ 2f0 right-left)
             m.03 (cl:- (cl:/ (cl:+ right left) right-left))
             m.11 (cl:/ 2f0 top-bottom)
             m.13 (cl:- (cl:/ (cl:+ top bottom) top-bottom))
             m.22 (cl:/ -2f0 far-near)
             m.23 (cl:- (cl:/ (cl:+ far near) far-near))))
    out))

(defspecialization (set-projection/orthographic :inline t) ((left real) (right real) (bottom real)
                                                            (top real) (near real) (far real)
                                                            (out null))
    mat4
  (declare (ignore out))
  (set-projection/orthographic left right bottom top near far (the mat4 (mat4 1))))

(defspecialization (set-projection/perspective :inline t) ((fov real) (aspect real) (near real)
                                                           (far real) (out mat4))
    mat4
  (let ((f (float (cl:/ (cl:tan (cl:/ fov 2))) 1f0))
        (z (float (cl:- near far) 1f0)))
    (with-mat4 ((m (zero out)))
      (psetf m.00 (cl:/ f aspect)
             m.11 f
             m.22 (cl:/ (cl:+ near far) z)
             m.23 (cl:/ (cl:* 2 near far) z)
             m.32 -1f0)))
  out)

(defspecialization (set-projection/perspective :inline t) ((fov real) (aspect real) (near real)
                                                           (far real) (out null))
    mat4
  (declare (ignore out))
  (set-projection/perspective fov aspect near far (the mat4 (mat4 1))))
