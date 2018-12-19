(in-package :first-light.math)

;;; constants

(fl.util:define-constant +zero-quat+
    (%quat 0f0 0f0 0f0 0f0)
  :test #'equalp)

(fl.util:define-constant +id-quat+
    (%quat 1f0 0f0 0f0 0f0)
  :test #'equalp)

;;; array accessor

(defspecialization (get-array :inline t) ((quat quat)) (simple-array single-float (4))
  (quat-array quat))

;;; component accessors

(defspecialization (w :inline t) ((quat quat)) single-float
  (with-quat ((q quat))
    q.w))

(defspecialization ((setf w) :inline t) ((value real) (quat quat)) single-float
  (with-quat ((q quat))
    (setf q.w (float value 1f0))))

(defspecialization (x :inline t) ((quat quat)) single-float
  (with-quat ((q quat))
    q.x))

(defspecialization ((setf x) :inline t) ((value real) (quat quat)) single-float
  (with-quat ((q quat))
    (setf q.x (float value 1f0))))

(defspecialization (y :inline t) ((quat quat)) single-float
  (with-quat ((q quat))
    q.y))

(defspecialization ((setf y) :inline t) ((value real) (quat quat)) single-float
  (with-quat ((q quat))
    (setf q.y (float value 1f0))))

(defspecialization (z :inline t) ((quat quat)) single-float
  (with-quat ((q quat))
    q.z))

(defspecialization ((setf z) :inline t) ((value real) (quat quat)) single-float
  (with-quat ((q quat))
    (setf q.z (float value 1f0))))

;;; constructors

(defspecialization (quat :inline t) () quat
  (%quat 0f0 0f0 0f0 0f0))

(defspecialization (quat :inline t) ((w real)) quat
  (%quat (float w 1f0) 0f0 0f0 0f0))

(defspecialization (quat :inline t) ((w real) (x real) (y real) (z real)) quat
  (%quat (float w 1f0) (float x 1f0) (float y 1f0) (float z 1f0)))

(defspecialization (quat :inline t) ((xyz vec3)) quat
  (%quat 0f0 (y xyz) (z xyz) (w xyz)))

(defspecialization (quat :inline t) ((xyzw vec4)) quat
  (%quat (x xyzw) (y xyzw) (z xyzw) (w xyzw)))

(defspecialization (quat :inline t) ((xyzw vec4)) quat
  (%quat (x xyzw) (y xyzw) (z xyzw) (w xyzw)))

(defspecialization (quat :inline t) ((in mat3)) quat
  (with-quat ((q (quat 1)))
    (with-mat3 ((m in))
      (let* ((x-rot-denom (cl:sqrt (cl:+ (cl:* m.00 m.00) (cl:* m.10 m.10) (cl:* m.20 m.20))))
             (y-rot-denom (cl:sqrt (cl:+ (cl:* m.01 m.01) (cl:* m.11 m.11) (cl:* m.21 m.21))))
             (z-rot-denom (cl:sqrt (cl:+ (cl:* m.02 m.02) (cl:* m.12 m.12) (cl:* m.22 m.22))))
             (nm00 (cl:/ m.00 x-rot-denom))
             (nm10 (cl:/ m.10 x-rot-denom))
             (nm20 (cl:/ m.20 x-rot-denom))
             (nm01 (cl:/ m.01 y-rot-denom))
             (nm11 (cl:/ m.11 y-rot-denom))
             (nm21 (cl:/ m.21 y-rot-denom))
             (nm02 (cl:/ m.02 z-rot-denom))
             (nm12 (cl:/ m.12 z-rot-denom))
             (nm22 (cl:/ m.22 z-rot-denom)))
        (let ((trace (cl:+ nm00 nm11 nm22 1))
              (col1 (1+ (cl:- nm00 nm11 nm22)))
              (col2 (1+ (cl:- nm11 nm00 nm22)))
              (col3 (1+ (cl:- nm22 nm00 nm11)))
              (s 0.0f0))
          (cond
            ((plusp trace)
             (setf s (cl:/ 0.5f0 (cl:sqrt trace))
                   q.w (cl:/ 0.25f0 s)
                   q.x (cl:* (cl:- nm21 nm12) s)
                   q.y (cl:* (cl:- nm02 nm20) s)
                   q.z (cl:* (cl:- nm10 nm01) s)))
            ((and (cl:>= col1 col2) (cl:>= col1 col3))
             (setf s (cl:/ 0.5f0 (cl:sqrt col1))
                   q.w (cl:* (cl:- nm21 nm12) s)
                   q.x (cl:/ 0.25f0 s)
                   q.y (cl:* (cl:+ nm10 nm01) s)
                   q.z (cl:* (cl:+ nm02 nm20) s)))
            ((and (cl:>= col2 col1) (cl:>= col2 col3))
             (setf s (cl:/ 0.5f0 (cl:sqrt col2))
                   q.w (cl:* (cl:- nm02 nm20) s)
                   q.x (cl:* (cl:+ nm01 nm10) s)
                   q.y (cl:/ 0.25f0 s)
                   q.z (cl:* (cl:+ nm12 nm21) s)))
            (t
             (setf s (cl:/ 0.5f0 (sqrt col3))
                   q.w (cl:* (cl:- nm10 nm01) s)
                   q.x (cl:* (cl:+ nm02 nm20) s)
                   q.y (cl:* (cl:+ nm12 nm21) s)
                   q.z (cl:/ 0.25f0 s)))))))
    q))

(defspecialization (quat :inline t) ((in mat4)) quat
  (with-quat ((q (quat 1)))
    (with-mat4 ((m in))
      (let* ((x-rot-denom (cl:sqrt (cl:+ (cl:* m.00 m.00) (cl:* m.10 m.10) (cl:* m.20 m.20))))
             (y-rot-denom (cl:sqrt (cl:+ (cl:* m.01 m.01) (cl:* m.11 m.11) (cl:* m.21 m.21))))
             (z-rot-denom (cl:sqrt (cl:+ (cl:* m.02 m.02) (cl:* m.12 m.12) (cl:* m.22 m.22))))
             (nm00 (cl:/ m.00 x-rot-denom))
             (nm10 (cl:/ m.10 x-rot-denom))
             (nm20 (cl:/ m.20 x-rot-denom))
             (nm01 (cl:/ m.01 y-rot-denom))
             (nm11 (cl:/ m.11 y-rot-denom))
             (nm21 (cl:/ m.21 y-rot-denom))
             (nm02 (cl:/ m.02 z-rot-denom))
             (nm12 (cl:/ m.12 z-rot-denom))
             (nm22 (cl:/ m.22 z-rot-denom)))
        (let ((trace (cl:+ nm00 nm11 nm22 m.33))
              (col1 (1+ (cl:- nm00 nm11 nm22)))
              (col2 (1+ (cl:- nm11 nm00 nm22)))
              (col3 (1+ (cl:- nm22 nm00 nm11)))
              (s 0.0f0))
          (cond
            ((plusp trace)
             (setf s (cl:/ 0.5f0 (cl:sqrt trace))
                   q.w (cl:/ 0.25f0 s)
                   q.x (cl:* (cl:- nm21 nm12) s)
                   q.y (cl:* (cl:- nm02 nm20) s)
                   q.z (cl:* (cl:- nm10 nm01) s)))
            ((and (cl:>= col1 col2) (cl:>= col1 col3))
             (setf s (cl:/ 0.5f0 (cl:sqrt col1))
                   q.w (cl:* (cl:- nm21 nm12) s)
                   q.x (cl:/ 0.25f0 s)
                   q.y (cl:* (cl:+ nm10 nm01) s)
                   q.z (cl:* (cl:+ nm02 nm20) s)))
            ((and (cl:>= col2 col1) (cl:>= col2 col3))
             (setf s (cl:/ 0.5f0 (cl:sqrt col2))
                   q.w (cl:* (cl:- nm02 nm20) s)
                   q.x (cl:* (cl:+ nm01 nm10) s)
                   q.y (cl:/ 0.25f0 s)
                   q.z (cl:* (cl:+ nm12 nm21) s)))
            (t
             (setf s (cl:/ 0.5f0 (sqrt col3))
                   q.w (cl:* (cl:- nm10 nm01) s)
                   q.x (cl:* (cl:+ nm02 nm20) s)
                   q.y (cl:* (cl:+ nm12 nm21) s)
                   q.z (cl:/ 0.25f0 s)))))))
    q))

;;; copiers

(defspecialization (copy :inline t) ((in quat)) quat
  (%quat (w in) (x in) (y in) (z in)))

(defspecialization (copy-into :inline t) ((out quat) (wxyz quat)) quat
  (with-quat ((o out) (q wxyz))
    (setf o.w q.w o.x q.x o.y q.y o.z q.z))
  out)

(defspecialization (copy-into :inline t) ((out quat) (xyz vec3)) quat
  (with-quat ((o out))
    (with-vec3 ((v xyz))
      (setf o.x v.x o.y v.y o.z v.z)))
  out)

(defspecialization (copy-into :inline t) ((out quat) (xyzw vec4)) quat
  (with-quat ((o out))
    (with-vec4 ((v xyzw))
      (setf o.w v.x o.x v.y o.y v.z o.z v.w)))
  out)

;;; operations

(defspecialization (zero :inline t) ((out quat)) quat
  (with-quat ((o out))
    (psetf o.w 0f0 o.x 0f0 o.y 0f0 o.z 0f0))
  out)

(defspecialization (zero-p :inline t) ((in quat)) boolean
  (with-quat ((q in))
    (cl:= 0f0 q.w q.x q.y q.z)))

(defspecialization (unit-p :inline t) ((in quat)) boolean
  (with-quat ((q in))
    (cl:= 0f0 (cl:- 1f0 (cl:+ (cl:expt q.w 2) (cl:expt q.x 2) (cl:expt q.y 2) (cl:expt q.z 2))))))

(defspecialization (+ :inline t) ((in1 quat) (in2 quat) (out quat)) quat
  (with-quat ((q1 in1) (q2 in2) (o out))
    (psetf o.w (cl:+ q1.w q2.w)
           o.x (cl:+ q1.x q2.x)
           o.y (cl:+ q1.y q2.y)
           o.z (cl:+ q1.z q2.z)))
  out)

(defspecialization (+ :inline t) ((in1 quat) (in2 quat) (out null)) quat
  (declare (ignore out))
  (+ in1 in2 (the quat (quat 1))))

(defspecialization (- :inline t) ((in1 quat) (in2 quat) (out quat)) quat
  (with-quat ((q1 in1) (q2 in2) (o out))
    (psetf o.w (cl:- q1.w q2.w)
           o.x (cl:- q1.x q2.x)
           o.y (cl:- q1.y q2.y)
           o.z (cl:- q1.z q2.z)))
  out)

(defspecialization (- :inline t) ((in1 quat) (in2 quat) (out null)) quat
  (declare (ignore out))
  (- in1 in2 (the quat (quat 1))))

(defspecialization (* :inline t) ((in1 quat) (in2 quat) (out quat)) quat
  (with-quat ((q1 in1) (q2 in2) (o out))
    (psetf o.w (cl:- (cl:* q1.w q2.w) (cl:* q1.x q2.x) (cl:* q1.y q2.y) (cl:* q1.z q2.z))
           o.x (cl:- (cl:+ (cl:* q1.w q2.x) (cl:* q1.x q2.w) (cl:* q1.y q2.z)) (cl:* q1.z q2.y))
           o.y (cl:- (cl:+ (cl:* q1.w q2.y) (cl:* q1.y q2.w) (cl:* q1.z q2.x)) (cl:* q1.x q2.z))
           o.z (cl:- (cl:+ (cl:* q1.w q2.z) (cl:* q1.z q2.w) (cl:* q1.x q2.y)) (cl:* q1.y q2.x))))
  out)

(defspecialization (* :inline t) ((in1 quat) (in2 quat) (out null)) quat
  (declare (ignore out))
  (* in1 in2 (the quat (quat 1))))

(defspecialization (* :inline t) ((in1 quat) (in2 real) (out quat)) quat
  (with-quat ((q in1) (o out))
    (psetf o.w (cl:* q.w in2)
           o.x (cl:* q.x in2)
           o.y (cl:* q.y in2)
           o.z (cl:* q.z in2)))
  out)

(defspecialization (* :inline t) ((in1 quat) (in2 real) (out null)) quat
  (declare (ignore out))
  (* in1 in2 (the quat (quat 1))))

(defspecialization (conjugate :inline t) ((quat quat) (out quat)) quat
  (with-quat ((q quat) (o out))
    (psetf o.w q.w
           o.x (cl:- q.x)
           o.y (cl:- q.y)
           o.z (cl:- q.z)))
  out)

(defspecialization (conjugate :inline t) ((quat quat) (out null)) quat
  (declare (ignore out))
  (conjugate quat (the quat (quat 1))))

(defspecialization (cross :inline t) ((in1 quat) (in2 quat) (out quat)) quat
  (* (+ (* in2 (conjugate in1)) (* in1 in2)) 0.5f0 out)
  out)

(defspecialization (cross :inline t) ((in1 quat) (in2 quat) (out null)) quat
  (declare (ignore out))
  (cross in1 in2 (the quat (quat 1))))

(defspecialization (length-squared :inline t) ((in quat)) single-float
  (with-quat ((q in))
    (cl:+ (cl:* q.w q.w) (cl:* q.x q.x) (cl:* q.y q.y) (cl:* q.z q.z))))

(defspecialization (length :inline t) ((in quat)) single-float
  (cl:sqrt (length-squared in)))

(defspecialization (normalize :inline t) ((in quat) (out quat)) quat
  (let ((length (length in)))
    (unless (zerop length)
      (* in (cl:/ length) out)))
  out)

(defspecialization (normalize :inline t) ((in quat) (out null)) quat
  (declare (ignore out))
  (normalize in (the quat (quat 1))))

(defspecialization (negate :inline t) ((in quat) (out quat)) quat
  (with-quat ((q in) (o out))
    (psetf o.w (cl:- q.w)
           o.x (cl:- q.x)
           o.y (cl:- q.y)
           o.z (cl:- q.z)))
  out)

(defspecialization (negate :inline t) ((in quat) (out null)) quat
  (declare (ignore out))
  (negate in (the quat (quat 1))))

(defspecialization (dot :inline t) ((in1 quat) (in2 quat)) single-float
  (with-quat ((q1 in1) (q2 in2))
    (cl:+ (cl:* q1.w q2.w)
          (cl:* q1.x q2.x)
          (cl:* q1.y q2.y)
          (cl:* q1.z q2.z))))

(defspecialization (inverse :inline t) ((in quat) (out quat)) quat
  (conjugate in out)
  (* out (cl:/ (length-squared in)) out)
  out)

(defspecialization (inverse :inline t) ((in quat) (out null)) quat
  (declare (ignore out))
  (inverse in (the quat (quat 1))))

(defspecialization (rotate :inline t) ((in quat) (angle vec3) (out quat)) quat
  (with-quat ((o out) (q (copy in)))
    (with-vec3 ((v (* angle 0.5f0))
                (c (vec3 (cl:cos v.x) (cl:cos v.y) (cl:cos v.z)))
                (s (vec3 (cl:sin v.x) (cl:sin v.y) (cl:sin v.z))))
      (psetf o.w (cl:- (cl:* c.x c.y c.z) (cl:* s.x s.y s.z))
             o.x (cl:+ (cl:* s.x c.y c.z) (cl:* c.x s.y s.z))
             o.y (cl:- (cl:* c.x s.y c.z) (cl:* s.x c.y s.z))
             o.z (cl:+ (cl:* s.x s.y c.z) (cl:* c.x c.y s.z)))
      (* out q out)))
  out)

(defspecialization (rotate :inline t) ((in quat) (angle vec3) (out null)) quat
  (declare (ignore out))
  (rotate in angle (the quat (quat 1))))

(defspecialization (slerp :inline t) ((in1 quat) (in2 quat) (factor real) (out quat)) quat
  (with-quat ((q1 in1) (q2 in2) (o out))
    (let ((dot (dot q1 q2))
          (q2 q2))
      (when (minusp dot)
        (negate q2 q2)
        (setf dot (cl:- dot)))
      (if (cl:> (cl:abs dot) 0.9995f0)
          (psetf o.w (fl.util:lerp factor q1.w q2.w)
                 o.x (fl.util:lerp factor q1.x q2.x)
                 o.y (fl.util:lerp factor q1.y q2.y)
                 o.z (fl.util:lerp factor q1.z q2.z))
          (let* ((angle (cl:acos (fl.util:clamp dot 0 1)))
                 (sin-angle (cl:sin angle))
                 (scale1 (cl:/ (cl:sin (cl:* angle (cl:- 1 factor))) sin-angle))
                 (scale2 (cl:/ (cl:sin (cl:* factor angle)) sin-angle)))
            (psetf o.w (cl:+ (cl:* q1.w scale1) (cl:* q2.w scale2))
                   o.x (cl:+ (cl:* q1.x scale1) (cl:* q2.x scale2))
                   o.y (cl:+ (cl:* q1.y scale1) (cl:* q2.y scale2))
                   o.z (cl:+ (cl:* q1.z scale1) (cl:* q2.z scale2)))))))
  out)

(defspecialization (slerp :inline t) ((in1 quat) (in2 quat) (factor real) (out null)) quat
  (declare (ignore out))
  (slerp in1 in2 factor (the quat (quat 1))))
