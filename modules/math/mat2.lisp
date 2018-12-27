(in-package :first-light.math)

;;; constants

(fl.util:define-constant +zero-mat2+
    (%mat2 0f0 0f0 0f0 0f0)
  :test #'equalp)

(fl.util:define-constant +id-mat2+
    (%mat2 1f0 0f0 0f0 1f0)
  :test #'equalp)

;;; array accessor

(defspecialization (get-array :inline t) ((matrix mat2)) (simple-array single-float (4))
  (m2-array matrix))

;;; constructors

(defspecialization (mat2 :inline t) () mat2
  (%mat2 0f0 0f0 0f0 0f0))

(defspecialization (mat2 :inline t) ((x real)) mat2
  (%mat2 (float x 1f0) 0f0 0f0 (float x 1f0)))

(defspecialization (mat2 :inline t) ((mat mat2)) mat2
  (with-mat2 ((m mat))
    (%mat2 m.00 m.10 m.01 m.11)))

(defspecialization (mat2 :inline t) ((mat mat3)) mat2
  (with-mat3 ((m mat))
    (%mat2 m.00 m.10 m.01 m.11)))

(defspecialization (mat2 :inline t) ((mat mat4)) mat2
  (with-mat4 ((m mat))
    (%mat2 m.00 m.10 m.01 m.11)))

(defspecialization (mat2 :inline t) ((a vec2) (b vec2)) mat2
  (with-vec2 ((a a) (b b))
    (%mat2 a.x a.y b.x b.y)))

(defspecialization (mat2 :inline t) ((m00 real) (m10 real) (m01 real) (m11 real)) mat2
  (%mat2 (float m00 1f0) (float m10 1f0)
         (float m01 1f0) (float m11 1f0)))

;;; copiers

(defspecialization (copy :inline t) ((in mat2)) mat2
  (with-mat2 ((m in))
    (%mat2 m.00 m.10 m.01 m.11)))

(defspecialization (copy :inline t) ((in mat2) (x real)) mat2
  (with-mat2 ((m in))
    (%mat2 (float x 1f0) m.10 m.01 (float x 1f0))))

(defspecialization (copy :inline t) ((in mat2) (x mat2)) mat2
  (with-mat2 ((m in) (x x))
    (%mat2 x.00 x.10 x.01 x.11)))

(defspecialization (copy :inline t) ((in mat2) (x mat3)) mat2
  (with-mat2 ((m in))
    (with-mat3 ((x x))
      (%mat2 x.00 x.10 x.01 x.11))))

(defspecialization (copy :inline t) ((in mat2) (x mat4)) mat2
  (declare (ignore in))
  (with-mat4 ((x x))
    (%mat2 x.00 x.10 x.01 x.11)))

(defspecialization (copy :inline t) ((in mat2) (a vec2) (b vec2)) mat2
  (declare (ignore in))
  (with-vec2 ((a a) (b b))
    (%mat2 a.x a.y b.x b.y)))

(defspecialization (copy :inline t) ((in mat2) (m00 real) (m10 real) (m01 real) (m11 real)) mat2
  (declare (ignore in))
  (%mat2 m00 m10 m01 m11))

(defspecialization (copy-into :inline t) ((out mat2)) mat2
  out)

(defspecialization (copy-into :inline t) ((out mat2) (x real)) mat2
  (with-mat2 ((o out))
    (setf o.00 (float x 1f0) o.11 (float x 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out mat2) (x mat2)) mat2
  (with-mat2 ((o out) (x x))
    (setf o.00 x.00 o.10 x.10 o.01 x.01 o.11 x.11))
  out)

(defspecialization (copy-into :inline t) ((out mat2) (x mat3)) mat2
  (with-mat2 ((o out))
    (with-mat3 ((x x))
      (setf o.00 x.00 o.10 x.10
            o.01 x.01 o.11 x.11)))
  out)

(defspecialization (copy-into :inline t) ((out mat2) (x mat4)) mat2
  (with-mat2 ((o out))
    (with-mat4 ((x x))
      (setf o.00 x.00 o.10 x.10
            o.01 x.01 o.11 x.11)))
  out)

(defspecialization (copy-into :inline t) ((out mat2) (a vec2) (b vec2)) mat2
  (with-mat2 ((o out))
    (with-vec2 ((a a) (b b))
      (setf o.00 a.x o.10 a.y
            o.01 b.x o.11 b.y)))
  out)

(defspecialization (copy-into :inline t) ((out mat2) (m00 real) (m01 real) (m10 real) (m11 real))
    mat2
  (with-mat2 ((o out))
    (setf o.00 m00 o.10 m10
          o.01 m01 o.11 m11))
  out)

;;; operations

(defspecialization (rand :inline t) ((in mat2) (min real) (max real) (out mat2)) mat2
  (declare (ignore in))
  (let ((min (float min 1f0))
        (max (float max 1f0)))
    (with-mat2 ((o out))
      (psetf o.00 (cl:+ min (random (cl:- max min)))
             o.01 (cl:+ min (random (cl:- max min)))
             o.10 (cl:+ min (random (cl:- max min)))
             o.11 (cl:+ min (random (cl:- max min))))))
  out)

(defspecialization (rand :inline t) ((in mat2) (min real) (max real) (out null)) mat2
  (declare (ignore out))
  (rand in min max (the mat2 (mat2))))

(defspecialization (zero :inline t) ((out mat2)) mat2
  (with-mat2 ((m out))
    (psetf m.00 0f0 m.10 0f0
           m.01 0f0 m.11 0f0))
  out)

(defspecialization (zero-p :inline t) ((in mat2)) boolean
  (with-mat2 ((m in))
    (cl:= 0f0 m.00 m.10 m.01 m.11)))

(defspecialization (id :inline t) ((out mat2)) mat2
  (with-mat2 ((m out))
    (psetf m.00 1f0 m.10 0f0
           m.01 0f0 m.11 1f0))
  out)

(defspecialization (id-p :inline t) ((in mat2)) boolean
  (with-mat2 ((m in))
    (and (cl:= 0f0 m.10 m.01)
         (cl:= 1f0 m.00 m.11))))

(defspecialization (clamp :inline t) ((in mat2) (min real) (max real) (out mat2)) mat2
  (with-mat2 ((m in) (o out))
    (psetf o.00 (float (fl.util:clamp m.00 min max) 1f0)
           o.01 (float (fl.util:clamp m.01 min max) 1f0)
           o.10 (float (fl.util:clamp m.10 min max) 1f0)
           o.11 (float (fl.util:clamp m.11 min max) 1f0)))
  out)

(defspecialization (clamp :inline t) ((in mat2) (min real) (max real) (out null)) mat2
  (declare (ignore out))
  (clamp in min max (the mat2 (mat2))))

(defspecialization (stabilize :inline t) ((in mat2) (tolerance single-float) (out mat2)) mat2
  (with-mat2 ((m in) (o out))
    (macrolet ((%stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0f0 ,place)))
      (psetf o.00 (%stabilize m.00)
             o.01 (%stabilize m.01)
             o.10 (%stabilize m.10)
             o.11 (%stabilize m.11))))
  out)

(defspecialization (stabilize :inline t) ((in mat2) (tolerance single-float) (out null)) mat2
  (declare (ignore out))
  (stabilize in tolerance (the mat2 (mat2))))

(defspecialization (+ :inline t) ((in1 mat2) (in2 mat2) (out mat2)) mat2
  (with-mat2 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:+ m1.00 m2.00)
           o.01 (cl:+ m1.01 m2.01)
           o.10 (cl:+ m1.10 m2.10)
           o.11 (cl:+ m1.11 m2.11)))
  out)

(defspecialization (+ :inline t) ((in1 mat2) (in2 mat2) (out null)) mat2
  (declare (ignore out))
  (+ in1 in2 (the mat2 (mat2))))

(defspecialization (- :inline t) ((in1 mat2) (in2 mat2) (out mat2)) mat2
  (with-mat2 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:- m1.00 m2.00)
           o.01 (cl:- m1.01 m2.01)
           o.10 (cl:- m1.10 m2.10)
           o.11 (cl:- m1.11 m2.11)))
  out)

(defspecialization (- :inline t) ((in1 mat2) (in2 mat2) (out null)) mat2
  (declare (ignore out))
  (- in1 in2 (the mat2 (mat2))))

(defspecialization (* :inline t) ((in1 mat2) (in2 mat2) (out mat2)) mat2
  (with-mat2 ((m1 in1) (m2 in2) (o out))
    (psetf o.00 (cl:+ (cl:* m1.00 m2.00) (cl:* m1.01 m2.10))
           o.01 (cl:+ (cl:* m1.00 m2.01) (cl:* m1.01 m2.11))
           o.10 (cl:+ (cl:* m1.10 m2.00) (cl:* m1.11 m2.10))
           o.11 (cl:+ (cl:* m1.10 m2.01) (cl:* m1.11 m2.11))))
  out)

(defspecialization (* :inline t) ((in1 mat2) (in2 mat2) (out null)) mat2
  (declare (ignore out))
  (* in1 in2 (the mat2 (mat2))))

(defspecialization (* :inline t) ((in1 mat2) (in2 vec2) (out vec2)) vec2
  (with-mat2 ((m in1))
    (with-vec2 ((v in2) (o out))
      (psetf o.x (cl:+ (cl:* m.00 v.x) (cl:* m.01 v.y))
             o.y (cl:+ (cl:* m.10 v.x) (cl:* m.11 v.y)))))
  out)

(defspecialization (* :inline t) ((in1 mat2) (in2 vec2) (out null)) vec2
  (declare (ignore out))
  (* in1 in2 (the vec2 (vec2))))

(defspecialization (* :inline t) ((in1 mat2) (in2 real) (out mat2)) mat2
  (with-mat2 ((m in1) (o out))
    (psetf o.00 (cl:* m.00 in2)
           o.01 (cl:* m.01 in2)
           o.10 (cl:* m.10 in2)
           o.11 (cl:* m.11 in2)))
  out)

(defspecialization (* :inline t) ((in1 mat2) (in2 real) (out null)) mat2
  (declare (ignore out))
  (* in1 in2 (the mat2 (mat2))))

(defspecialization (/ :inline t) ((in1 mat2) (in2 real) (out mat2)) mat2
  (with-mat2 ((m in1) (o out))
    (psetf o.00 (if (zerop in2) 0f0 (cl:/ m.00 in2))
           o.01 (if (zerop in2) 0f0 (cl:/ m.01 in2))
           o.10 (if (zerop in2) 0f0 (cl:/ m.10 in2))
           o.11 (if (zerop in2) 0f0 (cl:/ m.11 in2))))
  out)

(defspecialization (/ :inline t) ((in1 mat2) (in2 real) (out null)) mat2
  (declare (ignore out))
  (/ in1 in2 (the mat2 (mat2))))

(defspecialization (= :inline t) ((in1 mat2) (in2 mat2)) boolean
  (with-mat2 ((m1 in1) (m2 in2))
    (and (cl:= m1.00 m2.00) (cl:= m1.01 m2.01)
         (cl:= m1.10 m2.10) (cl:= m1.11 m2.11))))

(defspecialization (~ :inline t) ((in1 mat2) (in2 mat2)) boolean
  (with-mat2 ((m1 in1) (m2 in2))
    (and (cl:< (cl:abs (cl:- m1.00 m2.00)) 1e-7)
         (cl:< (cl:abs (cl:- m1.01 m2.01)) 1e-7)
         (cl:< (cl:abs (cl:- m1.10 m2.10)) 1e-7)
         (cl:< (cl:abs (cl:- m1.11 m2.11)) 1e-7))))

(defspecialization (get-column :inline t) ((mat mat2) (index (integer 0 1)) (out vec2)) vec2
  (with-mat2 ((m mat))
    (with-vec2 ((o out))
      (ecase index
        (0 (setf o.x m.00 o.y m.10))
        (1 (setf o.x m.01 o.y m.11)))))
  out)

(defspecialization (get-column :inline t) ((mat mat2) (index (integer 0 1)) (out null)) vec2
  (declare (ignore out))
  (get-column mat index (the vec2 (vec2))))

(defspecialization (set-column :inline t) ((mat mat2) (index (integer 0 1)) (vec vec2) (out mat2))
    mat2
  (with-mat2 ((o out))
    (with-vec2 ((v vec))
      (copy-into out mat)
      (ecase index
        (0 (setf o.00 v.x o.10 v.y))
        (1 (setf o.01 v.x o.11 v.y)))))
  out)

(defspecialization (set-column :inline t) ((mat mat2) (index (integer 0 1)) (vec vec2) (out null))
    mat2
  (declare (ignore out))
  (set-column mat index vec (the mat2 (mat2))))

(defspecialization (rotate :inline t) ((kind (eql :local)) (in mat2) (angle real) (out mat2)) mat2
  (declare (ignore kind))
  (with-mat2 ((m (mat2 1)))
    (copy-into out in)
    (when (cl:> (cl:abs angle) 1e-7)
      (let* ((angle (float angle 1f0))
             (s (cl:sin angle))
             (c (cl:cos angle)))
        (psetf m.00 c m.01 (cl:- s)
               m.10 s m.11 c)
        (* out m out))))
  out)

(defspecialization (rotate :inline t) ((kind (eql :local)) (in mat2) (angle real) (out null)) mat2
  (declare (ignore out))
  (rotate kind in angle (the mat2 (mat2 1))))

(defspecialization (get-scale :inline t) ((mat mat2) (out vec2)) vec2
  (with-mat2 ((m mat))
    (with-vec2 ((o out))
      (psetf o.x m.00 o.y m.11)))
  out)

(defspecialization (get-scale :inline t) ((mat mat2) (out null)) vec2
  (declare (ignore out))
  (get-scale mat (the vec2 (vec2))))

(defspecialization (set-scale :inline t) ((mat mat2) (vec vec2) (out mat2)) mat2
  (with-mat2 ((o out))
    (with-vec2 ((v vec))
      (copy-into out mat)
      (psetf o.00 v.x o.11 v.y)))
  out)

(defspecialization (set-scale :inline t) ((mat mat2) (vec vec2) (out null)) mat2
  (declare (ignore out))
  (set-scale mat vec (the mat2 (copy mat))))

(defspecialization (scale :inline t) ((mat mat2) (vec vec2) (out mat2)) mat2
  (* (the mat2 (set-scale (mat2 1) vec)) mat out))

(defspecialization (scale :inline t) ((mat mat2) (vec vec2) (out null)) mat2
  (declare (ignore out))
  (scale mat vec (the mat2 (mat2 1))))

(defspecialization (transpose :inline t) ((mat mat2) (out mat2)) mat2
  (with-mat2 ((o (copy-into out mat)))
    (rotatef o.01 o.10))
  out)

(defspecialization (transpose :inline t) ((mat mat2) (out null)) mat2
  (declare (ignore out))
  (transpose mat (the mat2 (mat2 1))))

(defspecialization (orthogonal-p :inline t) ((mat mat2)) boolean
  (~ (the mat2 (* mat (transpose mat))) (the mat2 (mat2 1))))

(defspecialization (trace :inline t) ((mat mat2)) single-float
  (with-mat2 ((m mat))
    (cl:+ m.00 m.11)))

(defspecialization (diagonal-p :inline t) ((mat mat2)) boolean
  (with-mat2 ((m mat))
    (cl:= 0f0 m.01 m.10)))

(defspecialization (main-diagonal :inline t) ((mat mat2) (out vec2)) vec2
  (with-mat2 ((m mat))
    (with-vec2 ((o out))
      (setf o.x m.00 o.y m.11)))
  out)

(defspecialization (main-diagonal :inline t) ((mat mat2) (out null)) vec2
  (declare (ignore out))
  (main-diagonal mat (the vec2 (vec2))))

(defspecialization (anti-diagonal :inline t) ((mat mat2) (out vec2)) vec2
  (with-mat2 ((m mat))
    (with-vec2 ((o out))
      (setf o.x m.01 o.y m.10)))
  out)

(defspecialization (anti-diagonal :inline t) ((mat mat2) (out null)) vec2
  (declare (ignore out))
  (anti-diagonal mat (the vec2 (vec2))))
