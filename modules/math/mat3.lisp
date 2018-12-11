(in-package :first-light.math)

;;; constants

(fl.util:define-constant +zero-mat3+
    (%mat3 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
  :test #'equalp)

(fl.util:define-constant +id-mat3+
    (%mat3 1f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 1f0)
  :test #'equalp)

;;; array accessor

(defspecialization (get-array :inline t) ((matrix mat3)) (simple-array single-float (9))
  (m3-array matrix))

;;; constructors

(defspecialization (mat3 :inline t) () mat3
  (%mat3 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))

(defspecialization (mat3 :inline t) ((x real)) mat3
  (%mat3 (float x 1f0) 0f0 0f0
         0f0 (float x 1f0) 0f0
         0f0 0f0 (float x 1f0)))

(defspecialization (mat3 :inline t) ((mat mat2)) mat3
  (with-mat2 ((m mat))
    (%mat3 m.00 m.10 0f0 m.01 m.11 0f0 0f0 0f0 1f0)))

(defspecialization (mat3 :inline t) ((mat mat3)) mat3
  (with-mat3 ((m mat))
    (%mat3 m.00 m.10 m.20 m.01 m.11 m.21 m.02 m.12 m.22)))

(defspecialization (mat3 :inline t) ((mat mat4)) mat3
  (with-mat4 ((m mat))
    (%mat3 m.00 m.10 m.20 m.01 m.11 m.21 m.02 m.12 m.22)))

(defspecialization (mat3 :inline t) ((a vec3) (b vec3) (c vec3)) mat3
  (with-vec3 ((a a) (b b) (c c))
    (%mat3 a.x a.y a.z b.x b.y b.z c.x c.y c.z)))

(defspecialization (mat3 :inline t) ((quat quat)) mat3
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
      (%mat3 (cl:- 1 (cl:+ yy zz)) (cl:+ xy wz) (cl:- xz wy)
             (cl:- xy wz) (cl:- 1 (cl:+ xx zz)) (cl:+ yz wx)
             (cl:+ xz wy) (cl:- yz wx) (cl:- 1 (cl:+ xx yy))))))

(defspecialization (mat3 :inline t) ((m00 real) (m10 real) (m20 real)
                                     (m01 real) (m11 real) (m21 real)
                                     (m02 real) (m12 real) (m22 real))
    mat3
  (%mat3 (float m00 1f0) (float m10 1f0) (float m20 1f0)
         (float m01 1f0) (float m11 1f0) (float m21 1f0)
         (float m02 1f0) (float m12 1f0) (float m22 1f0)))

;;; copiers

(defspecialization (copy :inline t) ((in mat3)) mat3
  (with-mat3 ((m in))
    (%mat3 m.00 m.10 m.20 m.01 m.11 m.21 m.02 m.12 m.22)))

(defspecialization (copy :inline t) ((in mat3) (x real)) mat3
  (with-mat3 ((m in))
    (%mat3 (float x 1f0) m.10 m.20
           m.01 (float x 1f0) m.21
           m.02 m.12 (float x 1f0))))

(defspecialization (copy :inline t) ((in mat3) (x mat2)) mat3
  (with-mat3 ((m in))
    (with-mat2 ((x x))
      (%mat3 x.00 x.10 m.20 x.01 x.11 m.21 m.02 m.12 m.22))))

(defspecialization (copy :inline t) ((in mat3) (x mat3)) mat3
  (with-mat3 ((m in) (x x))
    (%mat3 x.00 x.10 x.20 x.01 x.11 x.21 x.02 x.12 x.22)))

(defspecialization (copy :inline t) ((in mat3) (x mat4)) mat3
  (declare (ignore in))
  (with-mat4 ((x x))
    (%mat3 x.00 x.10 x.20 x.01 x.11 x.21 x.02 x.12 x.22)))

(defspecialization (copy :inline t) ((in mat3) (a vec3) (b vec3) (c vec3)) mat3
  (declare (ignore in))
  (with-vec3 ((a a) (b b) (c c))
    (%mat3 a.x a.y a.z b.x b.y b.z c.x c.y c.z)))

(defspecialization (copy :inline t) ((in mat3)
                                     (m00 real) (m10 real) (m20 real)
                                     (m01 real) (m11 real) (m21 real)
                                     (m02 real) (m12 real) (m22 real))
    mat3
  (declare (ignore in))
  (%mat3 m00 m10 m20 m01 m11 m21 m02 m12 m22))

(defspecialization (copy-into :inline t) ((out mat3)) mat3
  out)

(defspecialization (copy-into :inline t) ((out mat3) (x real)) mat3
  (with-mat3 ((o out))
    (setf o.00 (float x 1f0) o.11 (float x 1f0) o.22 (float x 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out mat3) (x mat2)) mat3
  (with-mat3 ((o out))
    (with-mat2 ((x x))
      (setf o.00 x.00 o.10 x.10 o.01 x.01 o.11 x.11)))
  out)

(defspecialization (copy-into :inline t) ((out mat3) (x mat3)) mat3
  (with-mat3 ((o out) (x x))
    (setf o.00 x.00 o.10 x.10 o.20 x.20
          o.01 x.01 o.11 x.11 o.21 x.21
          o.02 x.02 o.12 x.12 o.22 x.22))
  out)

(defspecialization (copy-into :inline t) ((out mat3) (x mat4)) mat3
  (with-mat3 ((o out))
    (with-mat4 ((x x))
      (setf o.00 x.00 o.10 x.10 o.20 x.20
            o.01 x.01 o.11 x.11 o.21 x.21
            o.02 x.02 o.12 x.12 o.22 x.22)))
  out)

(defspecialization (copy-into :inline t) ((out mat3) (a vec3) (b vec3) (c vec3)) mat3
  (with-mat3 ((o out))
    (with-vec3 ((a a) (b b) (c c))
      (setf o.00 a.x o.10 a.y o.20 a.z
            o.01 b.x o.11 b.y o.21 b.z
            o.02 c.x o.12 c.y o.22 c.z)))
  out)

(defspecialization (copy-into :inline t) ((out mat3)
                                          (m00 real) (m10 real) (m20 real)
                                          (m01 real) (m11 real) (m21 real)
                                          (m02 real) (m12 real) (m22 real))
    mat3
  (with-mat3 ((o out))
    (setf o.00 (float m00 1f0) o.10 (float m10 1f0) o.20 (float m20 1f0)
          o.01 (float m01 1f0) o.11 (float m11 1f0) o.21 (float m21 1f0)
          o.02 (float m02 1f0) o.12 (float m12 1f0) o.22 (float m22 1f0)))
  out)

;;; operations

(defspecialization (rand :inline t) ((in mat3) (min real) (max real) (out mat3)) mat3
  (declare (ignore in))
  (let ((min (float min 1f0))
        (max (float max 1f0)))
    (with-mat3 ((o out))
      (psetf o.00 (cl:+ min (random (cl:- max min)))
             o.01 (cl:+ min (random (cl:- max min)))
             o.02 (cl:+ min (random (cl:- max min)))
             o.10 (cl:+ min (random (cl:- max min)))
             o.11 (cl:+ min (random (cl:- max min)))
             o.12 (cl:+ min (random (cl:- max min)))
             o.20 (cl:+ min (random (cl:- max min)))
             o.21 (cl:+ min (random (cl:- max min)))
             o.22 (cl:+ min (random (cl:- max min))))))
  out)

(defspecialization (rand :inline t) ((in mat3) (min real) (max real) (out null)) mat3
  (declare (ignore out))
  (rand in min max (the mat3 (mat3))))

(defspecialization (zero :inline t) ((out mat3)) mat3
  (with-mat3 ((m out))
    (psetf m.00 0f0 m.10 0f0 m.20 0f0
           m.01 0f0 m.11 0f0 m.21 0f0
           m.02 0f0 m.12 0f0 m.22 0f0))
  out)

(defspecialization (zero-p :inline t) ((in mat3)) boolean
  (with-mat3 ((m in))
    (cl:= 0f0 m.00 m.10 m.20 m.01 m.11 m.21 m.02 m.12 m.22)))

(defspecialization (id :inline t) ((out mat3)) mat3
  (with-mat3 ((m out))
    (psetf m.00 1f0 m.10 0f0 m.20 0f0
           m.01 0f0 m.11 1f0 m.21 0f0
           m.02 0f0 m.12 0f0 m.22 1f0))
  out)

(defspecialization (id-p :inline t) ((in mat3)) boolean
  (with-mat3 ((m in))
    (and (cl:= 0f0 m.10 m.20 m.01 m.21 m.02 m.12)
         (cl:= 1f0 m.00 m.11 m.22))))

(defspecialization (clamp :inline t) ((in mat3) (min real) (max real) (out mat3)) mat3
  (with-mat3 ((m in) (o out))
    (psetf o.00 (float (fl.util:clamp m.00 min max) 1f0)
           o.01 (float (fl.util:clamp m.01 min max) 1f0)
           o.02 (float (fl.util:clamp m.02 min max) 1f0)
           o.10 (float (fl.util:clamp m.10 min max) 1f0)
           o.11 (float (fl.util:clamp m.11 min max) 1f0)
           o.12 (float (fl.util:clamp m.12 min max) 1f0)
           o.20 (float (fl.util:clamp m.20 min max) 1f0)
           o.21 (float (fl.util:clamp m.21 min max) 1f0)
           o.22 (float (fl.util:clamp m.22 min max) 1f0)))
  out)

(defspecialization (clamp :inline t) ((in mat3) (min real) (max real) (out null)) mat3
  (declare (ignore out))
  (clamp in min max (the mat3 (mat3))))

(defspecialization (stabilize :inline t) ((in mat3) (tolerance single-float) (out mat3)) mat3
  (with-mat3 ((m in) (o out))
    (macrolet ((%stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0f0 ,place)))
      (psetf o.00 (%stabilize m.00)
             o.01 (%stabilize m.01)
             o.02 (%stabilize m.02)
             o.10 (%stabilize m.10)
             o.11 (%stabilize m.11)
             o.12 (%stabilize m.12)
             o.20 (%stabilize m.20)
             o.21 (%stabilize m.21)
             o.22 (%stabilize m.22))))
  out)

(defspecialization (stabilize :inline t) ((in mat3) (tolerance single-float) (out null)) mat3
  (declare (ignore out))
  (stabilize in tolerance (the mat3 (mat3))))

(defspecialization (+ :inline t) ((in1 mat3) (in2 mat3) (out mat3)) mat3
  (with-mat3 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:+ m1.00 m2.00)
           o.01 (cl:+ m1.01 m2.01)
           o.02 (cl:+ m1.02 m2.02)
           o.10 (cl:+ m1.10 m2.10)
           o.11 (cl:+ m1.11 m2.11)
           o.12 (cl:+ m1.12 m2.12)
           o.20 (cl:+ m1.20 m2.20)
           o.21 (cl:+ m1.21 m2.21)
           o.22 (cl:+ m1.22 m2.22)))
  out)

(defspecialization (+ :inline t) ((in1 mat3) (in2 mat3) (out null)) mat3
  (declare (ignore out))
  (+ in1 in2 (the mat3 (mat3))))

(defspecialization (- :inline t) ((in1 mat3) (in2 mat3) (out mat3)) mat3
  (with-mat3 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:- m1.00 m2.00)
           o.01 (cl:- m1.01 m2.01)
           o.02 (cl:- m1.02 m2.02)
           o.10 (cl:- m1.10 m2.10)
           o.11 (cl:- m1.11 m2.11)
           o.12 (cl:- m1.12 m2.12)
           o.20 (cl:- m1.20 m2.20)
           o.21 (cl:- m1.21 m2.21)
           o.22 (cl:- m1.22 m2.22)))
  out)

(defspecialization (- :inline t) ((in1 mat3) (in2 mat3) (out null)) mat3
  (declare (ignore out))
  (- in1 in2 (the mat3 (mat3))))

(defspecialization (* :inline t) ((in1 mat3) (in2 mat3) (out mat3)) mat3
  (with-mat3 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:+ (cl:* m1.00 m2.00) (cl:* m1.01 m2.10) (cl:* m1.02 m2.20))
           o.01 (cl:+ (cl:* m1.00 m2.01) (cl:* m1.01 m2.11) (cl:* m1.02 m2.21))
           o.02 (cl:+ (cl:* m1.00 m2.02) (cl:* m1.01 m2.12) (cl:* m1.02 m2.22))
           o.10 (cl:+ (cl:* m1.10 m2.00) (cl:* m1.11 m2.10) (cl:* m1.12 m2.20))
           o.11 (cl:+ (cl:* m1.10 m2.01) (cl:* m1.11 m2.11) (cl:* m1.12 m2.21))
           o.12 (cl:+ (cl:* m1.10 m2.02) (cl:* m1.11 m2.12) (cl:* m1.12 m2.22))
           o.20 (cl:+ (cl:* m1.20 m2.00) (cl:* m1.21 m2.10) (cl:* m1.22 m2.20))
           o.21 (cl:+ (cl:* m1.20 m2.01) (cl:* m1.21 m2.11) (cl:* m1.22 m2.21))
           o.22 (cl:+ (cl:* m1.20 m2.02) (cl:* m1.21 m2.12) (cl:* m1.22 m2.22))))
  out)

(defspecialization (* :inline t) ((in1 mat3) (in2 mat3) (out null)) mat3
  (declare (ignore out))
  (* in1 in2 (the mat3 (mat3))))

(defspecialization (* :inline t) ((in1 mat3) (in2 vec3) (out vec3)) vec3
  (with-mat3 ((m in1))
    (with-vec3 ((v in2) (o out))
      (psetf o.x (cl:+ (cl:* m.00 v.x) (cl:* m.01 v.y) (cl:* m.02 v.z))
             o.y (cl:+ (cl:* m.10 v.x) (cl:* m.11 v.y) (cl:* m.12 v.z))
             o.z (cl:+ (cl:* m.20 v.x) (cl:* m.21 v.y) (cl:* m.22 v.z)))))
  out)

(defspecialization (* :inline t) ((in1 mat3) (in2 vec3) (out null)) vec3
  (declare (ignore out))
  (* in1 in2 (the vec3 (vec3))))

(defspecialization (* :inline t) ((in1 mat3) (in2 real) (out mat3)) mat3
  (with-mat3 ((m in1) (o out))
    (psetf o.00 (cl:* m.00 in2)
           o.01 (cl:* m.01 in2)
           o.02 (cl:* m.02 in2)
           o.10 (cl:* m.10 in2)
           o.11 (cl:* m.11 in2)
           o.12 (cl:* m.12 in2)
           o.20 (cl:* m.20 in2)
           o.21 (cl:* m.21 in2)
           o.22 (cl:* m.22 in2)))
  out)

(defspecialization (* :inline t) ((in1 mat3) (in2 real) (out null)) mat3
  (declare (ignore out))
  (* in1 in2 (the mat3 (mat3))))

(defspecialization (/ :inline t) ((in1 mat3) (in2 real) (out mat3)) mat3
  (with-mat3 ((m in1) (o out))
    (psetf o.00 (if (zerop in2) 0f0 (cl:/ m.00 in2))
           o.01 (if (zerop in2) 0f0 (cl:/ m.01 in2))
           o.02 (if (zerop in2) 0f0 (cl:/ m.02 in2))
           o.10 (if (zerop in2) 0f0 (cl:/ m.10 in2))
           o.11 (if (zerop in2) 0f0 (cl:/ m.11 in2))
           o.12 (if (zerop in2) 0f0 (cl:/ m.12 in2))
           o.20 (if (zerop in2) 0f0 (cl:/ m.20 in2))
           o.21 (if (zerop in2) 0f0 (cl:/ m.21 in2))
           o.22 (if (zerop in2) 0f0 (cl:/ m.22 in2))))
  out)

(defspecialization (/ :inline t) ((in1 mat3) (in2 real) (out null)) mat3
  (declare (ignore out))
  (/ in1 in2 (the mat3 (mat3))))

(defspecialization (= :inline t) ((in1 mat3) (in2 mat3)) boolean
  (with-mat3 ((m1 in1) (m2 in2))
    (and (cl:= m1.00 m2.00) (cl:= m1.01 m2.01) (cl:= m1.02 m2.02)
         (cl:= m1.10 m2.10) (cl:= m1.11 m2.11) (cl:= m1.12 m2.12)
         (cl:= m1.20 m2.20) (cl:= m1.21 m2.21) (cl:= m1.22 m2.22))))

(defspecialization (~ :inline t) ((in1 mat3) (in2 mat3)) boolean
  (with-mat3 ((m1 in1) (m2 in2))
    (and (cl:< (cl:abs (cl:- m1.00 m2.00)) 1e-7)
         (cl:< (cl:abs (cl:- m1.01 m2.01)) 1e-7)
         (cl:< (cl:abs (cl:- m1.02 m2.02)) 1e-7)
         (cl:< (cl:abs (cl:- m1.10 m2.10)) 1e-7)
         (cl:< (cl:abs (cl:- m1.11 m2.11)) 1e-7)
         (cl:< (cl:abs (cl:- m1.12 m2.12)) 1e-7)
         (cl:< (cl:abs (cl:- m1.20 m2.20)) 1e-7)
         (cl:< (cl:abs (cl:- m1.21 m2.21)) 1e-7)
         (cl:< (cl:abs (cl:- m1.22 m2.22)) 1e-7))))

(defspecialization (get-column :inline t) ((mat mat3) (index (integer 0 2)) (out vec3)) vec3
  (with-mat3 ((m mat))
    (with-vec3 ((o out))
      (ecase index
        (0 (setf o.x m.00 o.y m.10 o.z m.20))
        (1 (setf o.x m.01 o.y m.11 o.z m.21))
        (2 (setf o.x m.02 o.y m.12 o.z m.22)))))
  out)

(defspecialization (get-column :inline t) ((mat mat3) (index (integer 0 2)) (out null)) vec3
  (declare (ignore out))
  (get-column mat index (the vec3 (vec3))))

(defspecialization (set-column :inline t) ((mat mat3) (index (integer 0 2)) (vec vec3) (out mat3))
    mat3
  (with-mat3 ((o out))
    (with-vec3 ((v vec))
      (copy-into out mat)
      (ecase index
        (0 (setf o.00 v.x o.10 v.y o.20 v.z))
        (1 (setf o.01 v.x o.11 v.y o.21 v.z))
        (2 (setf o.02 v.x o.12 v.y o.22 v.z)))))
  out)

(defspecialization (set-column :inline t) ((mat mat3) (index (integer 0 2)) (vec vec3) (out null))
    mat3
  (declare (ignore out))
  (set-column mat index vec (the mat3 (mat3))))

(defspecialization (get-translation :inline t) ((mat mat3) (out vec2)) vec2
  (with-mat3 ((m mat))
    (with-vec2 ((o out))
      (psetf o.x m.20 o.y m.21)))
  out)

(defspecialization (get-translation :inline t) ((mat mat3) (out null)) vec2
  (declare (ignore out))
  (get-translation mat (the vec2 (vec2))))

(defspecialization (set-translation :inline t) ((mat mat3) (vec vec2) (out mat3)) mat3
  (with-mat3 ((o out))
    (with-vec2 ((v vec))
      (copy-into out mat)
      (psetf o.02 v.x o.12 v.y)))
  out)

(defspecialization (set-translation :inline t) ((mat mat3) (vec vec2) (out null)) mat3
  (declare (ignore out))
  (set-translation mat vec (the mat3 (copy mat))))

(defspecialization (translate :inline t) ((mat mat3) (vec vec2) (out mat3)) mat3
  (* (the mat3 (set-translation (mat3 1) vec)) mat out))

(defspecialization (translate :inline t) ((mat mat3) (vec vec2) (out null)) mat3
  (declare (ignore out))
  (translate mat vec (the mat3 (mat3 1))))

(defspecialization (rotate :inline t) ((in mat3) (angle vec3) (out mat3)) mat3
  (macrolet ((rotate-angle (angle s c &body body)
               `(when (cl:> (cl:abs ,angle) 1e-7)
                  (let ((,s (cl:sin ,angle))
                        (,c (cl:cos ,angle)))
                    ,@body
                    (* out m out)))))
    (with-mat3 ((m (mat3 1)))
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

(defspecialization (rotate :inline t) ((in mat3) (angle vec3) (out null)) mat3
  (declare (ignore out))
  (rotate in angle (the mat3 (mat3 1))))

(defspecialization (rotate :inline t) ((in mat3) (angle real) (out mat3)) mat3
  (with-mat3 ((m (mat3 1)))
    (copy-into out in)
    (when (cl:> (cl:abs angle) 1e-7)
      (let* ((angle (float angle 1f0))
             (s (cl:sin angle))
             (c (cl:cos angle)))
        (psetf m.00 c m.01 (cl:- s)
               m.10 s m.11 c)
        (* out m out))))
  out)

(defspecialization (rotate :inline t) ((in mat3) (angle real) (out null)) mat3
  (declare (ignore out))
  (rotate in angle (the mat3 (mat3 1))))

(defspecialization (get-scale :inline t) ((mat mat3) (out vec2)) vec2
  (with-mat3 ((m mat))
    (with-vec2 ((o out))
      (psetf o.x m.00 o.y m.11)))
  out)

(defspecialization (get-scale :inline t) ((mat mat3) (out null)) vec2
  (declare (ignore out))
  (get-scale mat (the vec2 (vec2))))

(defspecialization (set-scale :inline t) ((mat mat3) (vec vec2) (out mat3)) mat3
  (with-mat3 ((o out))
    (with-vec2 ((v vec))
      (copy-into out mat)
      (psetf o.00 v.x o.11 v.y)))
  out)

(defspecialization (set-scale :inline t) ((mat mat3) (vec vec2) (out null)) mat3
  (declare (ignore out))
  (set-scale mat vec (the mat3 (copy mat))))

(defspecialization (scale :inline t) ((mat mat3) (vec vec2) (out mat3)) mat3
  (* (the mat3 (set-scale (mat3 1) vec)) mat out))

(defspecialization (scale :inline t) ((mat mat3) (vec vec2) (out null)) mat3
  (declare (ignore out))
  (scale mat vec (the mat3 (mat3 1))))

(defspecialization (transpose :inline t) ((mat mat3) (out mat3)) mat3
  (with-mat3 ((o (copy-into out mat)))
    (rotatef o.01 o.10)
    (rotatef o.02 o.20)
    (rotatef o.12 o.21))
  out)

(defspecialization (transpose :inline t) ((mat mat3) (out null)) mat3
  (declare (ignore out))
  (transpose mat (the mat3 (mat3 1))))

(defspecialization (orthogonal-p :inline t) ((mat mat3)) boolean
  (~ (the mat3 (* mat (transpose mat))) (the mat3 (mat3 1))))

(defspecialization (orthonormalize :inline t) ((mat mat3) (out mat3)) mat3
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

(defspecialization (orthonormalize :inline t) ((mat mat3) (out null)) mat3
  (declare (ignore out))
  (orthonormalize mat (the mat3 (mat3 1))))

(defspecialization (trace :inline t) ((mat mat3)) single-float
  (with-mat3 ((m mat))
    (cl:+ m.00 m.11 m.22)))

(defspecialization (diagonal-p :inline t) ((mat mat3)) boolean
  (with-mat3 ((m mat))
    (cl:= 0f0 m.01 m.02 m.10 m.12 m.20 m.21)))

(defspecialization (main-diagonal :inline t) ((mat mat3) (out vec3)) vec3
  (with-mat3 ((m mat))
    (with-vec3 ((o out))
      (setf o.x m.00 o.y m.11 o.z m.22)))
  out)

(defspecialization (main-diagonal :inline t) ((mat mat3) (out null)) vec3
  (declare (ignore out))
  (main-diagonal mat (the vec3 (vec3))))

(defspecialization (anti-diagonal :inline t) ((mat mat3) (out vec3)) vec3
  (with-mat3 ((m mat))
    (with-vec3 ((o out))
      (setf o.x m.02 o.y m.11 o.z m.20)))
  out)

(defspecialization (anti-diagonal :inline t) ((mat mat3) (out null)) vec3
  (declare (ignore out))
  (anti-diagonal mat (the vec3 (vec3))))
