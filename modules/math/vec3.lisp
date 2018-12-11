(in-package :fl.math)

;;; constants

(fl.util:define-constant +zero-vec3+
    (%vec3 0f0 0f0 0f0)
  :test #'equalp)

;;; array accessor

(defspecialization (get-array :inline t) ((vec vec3)) (simple-array single-float (3))
  (v3-array vec))

;;; component accessors

(defspecialization (x :inline t) ((vec vec3)) single-float
  (with-vec3 ((v vec))
    v.x))

(defspecialization ((setf x) :inline t) ((value real) (vec vec3)) single-float
  (with-vec3 ((v vec))
    (setf v.x (float value 1f0))))

(defspecialization (y :inline t) ((vec vec3)) single-float
  (with-vec3 ((v vec))
    v.y))

(defspecialization ((setf y) :inline t) ((value real) (vec vec3)) single-float
  (with-vec3 ((v vec))
    (setf v.y (float value 1f0))))

(defspecialization (z :inline t) ((vec vec3)) single-float
  (with-vec3 ((v vec))
    v.z))

(defspecialization ((setf z) :inline t) ((value real) (vec vec3)) single-float
  (with-vec3 ((v vec))
    (setf v.z (float value 1f0))))

;;; constructors

(defspecialization (vec3 :inline t) () vec3
  (%vec3 0f0 0f0 0f0))

(defspecialization (vec3 :inline t) ((x real)) vec3
  (%vec3 (float x 1f0) (float x 1f0) (float x 1f0)))

(defspecialization (vec3 :inline t) ((xy vec2)) vec3
  (%vec3 (x xy) (y xy) 0f0))

(defspecialization (vec3 :inline t) ((xyz vec3)) vec3
  (%vec3 (x xyz) (y xyz) (z xyz)))

(defspecialization (vec3 :inline t) ((xyzw vec4)) vec3
  (%vec3 (x xyzw) (y xyzw) (z xyzw)))

(defspecialization (vec3 :inline t) ((x real) (y real)) vec3
  (%vec3 (float x 1f0) (float y 1f0) 0f0))

(defspecialization (vec3 :inline t) ((xy vec2) (z real)) vec3
  (%vec3 (x xy) (y xy) (float z 1f0)))

(defspecialization (vec3 :inline t) ((x real) (yz vec2)) vec3
  (%vec3 (float x 1f0) (x yz) (y yz)))

(defspecialization (vec3 :inline t) ((x real) (y real) (z real)) vec3
  (%vec3 (float x 1f0) (float y 1f0) (float z 1f0)))

(defspecialization (vec3 :inline t) ((wxyz quat)) vec3
  (%vec3 (x wxyz) (y wxyz) (z wxyz)))

;;; copiers

(defspecialization (copy :inline t) ((in vec3)) vec3
  (%vec3 (x in) (y in) (z in)))

(defspecialization (copy :inline t) ((in vec3) (x real)) vec3
  (%vec3 (float x 1f0) (y in) (z in)))

(defspecialization (copy :inline t) ((in vec3) (xy vec2)) vec3
  (%vec3 (x xy) (y xy) (z in)))

(defspecialization (copy :inline t) ((in vec3) (xyz vec3)) vec3
  (declare (ignore in))
  (%vec3 (x xyz) (y xyz) (z xyz)))

(defspecialization (copy :inline t) ((in vec3) (xyzw vec4)) vec3
  (declare (ignore in))
  (%vec3 (x xyzw) (y xyzw) (z xyzw)))

(defspecialization (copy :inline t) ((in vec3) (x real) (y real)) vec3
  (%vec3 (float x 1f0) (float y 1f0) (z in)))

(defspecialization (copy :inline t) ((in vec3) (xy vec2) (z real)) vec3
  (declare (ignore in))
  (%vec3 (x xy) (y xy) (float z 1f0)))

(defspecialization (copy :inline t) ((in vec3) (x real) (yz vec2)) vec3
  (declare (ignore in))
  (%vec3 (float x 1f0) (x yz) (y yz)))

(defspecialization (copy :inline t) ((in vec3) (x real) (y real) (z real)) vec3
  (declare (ignore in))
  (%vec3 (float x 1f0) (float y 1f0) (float z 1f0)))

(defspecialization (copy-into :inline t) ((out vec3)) vec3
  out)

(defspecialization (copy-into :inline t) ((out vec3) (x real)) vec3
  (with-vec3 ((o out))
    (setf o.x (float x 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out vec3) (xy vec2)) vec3
  (with-vec3 ((o out))
    (with-vec2 ((v xy))
      (setf o.x v.x o.y v.y)))
  out)

(defspecialization (copy-into :inline t) ((out vec3) (xyz vec3)) vec3
  (with-vec3 ((o out) (v xyz))
    (setf o.x v.x o.y v.y o.z v.z))
  out)

(defspecialization (copy-into :inline t) ((out vec3) (xyzw vec4)) vec3
  (with-vec3 ((o out))
    (with-vec4 ((v xyzw))
      (setf o.x v.x o.y v.y o.z v.z)))
  out)

(defspecialization (copy-into :inline t) ((out vec3) (x real) (y real)) vec3
  (with-vec3 ((o out))
    (setf o.x (float x 1f0) o.y (float y 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out vec3) (xy vec2) (z real)) vec3
  (with-vec3 ((o out))
    (with-vec2 ((v xy))
      (setf o.x v.x o.y v.y o.z (float z 1f0))))
  out)

(defspecialization (copy-into :inline t) ((out vec3) (x real) (yz vec2)) vec3
  (with-vec3 ((o out))
    (with-vec2 ((v yz))
      (setf o.x (float x 1f0) o.y v.x o.z v.y)))
  out)

(defspecialization (copy-into :inline t) ((out vec3) (x real) (y real) (z real)) vec3
  (with-vec3 ((o out))
    (setf o.x (float x 1f0) o.y (float y 1f0) o.z (float z 1f0)))
  out)

;;; operations

(defspecialization (rand :inline t) ((in vec3) (min real) (max real) (out vec3)) vec3
  (declare (ignore in))
  (let ((min (float min 1f0))
        (max (float max 1f0)))
    (with-vec3 ((o out))
      (psetf o.x (cl:+ min (random (cl:- max min)))
             o.y (cl:+ min (random (cl:- max min)))
             o.z (cl:+ min (random (cl:- max min))))))
  out)

(defspecialization (rand :inline t) ((in vec3) (min real) (max real) (out null)) vec3
  (declare (ignore out))
  (rand in min max (the vec3 (vec3))))

(defspecialization (zero :inline t) ((out vec3)) vec3
  (with-vec3 ((o out))
    (psetf o.x 0f0 o.y 0f0 o.z 0f0))
  out)

(defspecialization (zero-p :inline t) ((in vec3)) boolean
  (with-vec3 ((v in))
    (cl:= 0f0 v.x v.y v.z)))

(defspecialization (unit-p :inline t) ((in vec3)) boolean
  (with-vec3 ((v in))
    (cl:= 0f0 (cl:- 1f0 (cl:+ (cl:expt v.x 2) (cl:expt v.y 2) (cl:expt v.z 2))))))

(defspecialization (clamp :inline t) ((in vec3) (min real) (max real) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (float (fl.util:clamp v.x min max) 1f0)
           o.y (float (fl.util:clamp v.y min max) 1f0)
           o.z (float (fl.util:clamp v.z min max) 1f0)))
  out)

(defspecialization (clamp :inline t) ((in vec3) (min real) (max real) (out null)) vec3
  (declare (ignore out))
  (clamp in min max (the vec3 (vec3))))

(defspecialization (stabilize :inline t) ((in vec3) (tolerance single-float) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (macrolet ((%stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0f0 ,place)))
      (psetf o.x (%stabilize v.x)
             o.y (%stabilize v.y)
             o.z (%stabilize v.z))))
  out)

(defspecialization (stabilize :inline t) ((in vec3) (tolerance single-float) (out null)) vec3
  (declare (ignore out))
  (stabilize in tolerance (the vec3 (vec3))))

(defspecialization (+ :inline t) ((in1 vec3) (in2 vec3) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:+ v1.x v2.x)
           o.y (cl:+ v1.y v2.y)
           o.z (cl:+ v1.z v2.z)))
  out)

(defspecialization (+ :inline t) ((in1 vec3) (in2 vec3) (out null)) vec3
  (declare (ignore out))
  (+ in1 in2 (the vec3 (vec3))))

(defspecialization (+ :inline t) ((in1 vec3) (in2 real) (out vec3)) vec3
  (with-vec3 ((v in1) (o out))
    (psetf o.x (cl:+ v.x in2)
           o.y (cl:+ v.y in2)
           o.z (cl:+ v.z in2)))
  out)

(defspecialization (+ :inline t) ((in1 vec3) (in2 real) (out null)) vec3
  (declare (ignore out))
  (+ in1 in2 (the vec3 (vec3))))

(defspecialization (- :inline t) ((in1 vec3) (in2 vec3) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:- v1.x v2.x)
           o.y (cl:- v1.y v2.y)
           o.z (cl:- v1.z v2.z)))
  out)

(defspecialization (- :inline t) ((in1 vec3) (in2 vec3) (out null)) vec3
  (declare (ignore out))
  (- in1 in2 (the vec3 (vec3))))

(defspecialization (- :inline t) ((in1 vec3) (in2 real) (out vec3)) vec3
  (with-vec3 ((v in1) (o out))
    (psetf o.x (cl:- v.x in2)
           o.y (cl:- v.y in2)
           o.z (cl:- v.z in2)))
  out)

(defspecialization (- :inline t) ((in1 vec3) (in2 real) (out null)) vec3
  (declare (ignore out))
  (- in1 in2 (the vec3 (vec3))))

(defspecialization (* :inline t) ((in1 vec3) (in2 vec3) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:* v1.x v2.x)
           o.y (cl:* v1.y v2.y)
           o.z (cl:* v1.z v2.z)))
  out)

(defspecialization (* :inline t) ((in1 vec3) (in2 vec3) (out null)) vec3
  (declare (ignore out))
  (* in1 in2 (the vec3 (vec3))))

(defspecialization (* :inline t) ((in1 vec3) (in2 real) (out vec3)) vec3
  (with-vec3 ((v in1) (o out))
    (psetf o.x (cl:* v.x in2)
           o.y (cl:* v.y in2)
           o.z (cl:* v.z in2)))
  out)

(defspecialization (* :inline t) ((in1 vec3) (in2 real) (out null)) vec3
  (declare (ignore out))
  (* in1 in2 (the vec3 (vec3))))

(defspecialization (/ :inline t) ((in1 vec3) (in2 vec3) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (if (zerop v2.x) 0f0 (cl:/ v1.x v2.x))
           o.y (if (zerop v2.y) 0f0 (cl:/ v1.y v2.y))
           o.z (if (zerop v2.z) 0f0 (cl:/ v1.z v2.z))))
  out)

(defspecialization (/ :inline t) ((in1 vec3) (in2 vec3) (out null)) vec3
  (declare (ignore out))
  (/ in1 in2 (the vec3 (vec3))))

(defspecialization (/ :inline t) ((in1 vec3) (in2 real) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (if (zerop in2) 0f0 (cl:/ v1.x in2))
           o.y (if (zerop in2) 0f0 (cl:/ v1.y in2))
           o.z (if (zerop in2) 0f0 (cl:/ v1.z in2))))
  out)

(defspecialization (/ :inline t) ((in1 vec3) (in2 real) (out null)) vec3
  (declare (ignore out))
  (/ in1 in2 (the vec3 (vec3))))

(defspecialization (sign :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (float (signum v.x) 1f0)
           o.y (float (signum v.y) 1f0)
           o.z (float (signum v.z) 1f0)))
  out)

(defspecialization (sign :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (sign in (the vec3 (vec3))))

(defspecialization (fract :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (nth-value 1 (cl:floor v.x))
           o.y (nth-value 1 (cl:floor v.y))
           o.z (nth-value 1 (cl:floor v.z))))
  out)

(defspecialization (fract :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (fract in (the vec3 (vec3))))

(defspecialization (dot :inline t) ((in1 vec3) (in2 vec3)) single-float
  (with-vec3 ((v1 in1) (v2 in2))
    (cl:+ (cl:* v1.x v2.x)
          (cl:* v1.y v2.y)
          (cl:* v1.z v2.z))))

(defspecialization (cross :inline t) ((in1 vec3) (in2 vec3) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:- (cl:* v1.y v2.z) (cl:* v1.z v2.y))
           o.y (cl:- (cl:* v1.z v2.x) (cl:* v1.x v2.z))
           o.z (cl:- (cl:* v1.x v2.y) (cl:* v1.y v2.x))))
  out)

(defspecialization (cross :inline t) ((in1 vec3) (in2 vec3) (out null)) vec3
  (declare (ignore out))
  (cross in1 in2 (the vec3 (vec3))))

(defspecialization (length-squared :inline t) ((in vec3)) single-float
  (dot in in))

(defspecialization (length :inline t) ((in vec3)) single-float
  (cl:sqrt (length-squared in)))

(defspecialization (distance-squared :inline t) ((in1 vec3) (in2 vec3)) single-float
  (length-squared (the vec3 (- in2 in1))))

(defspecialization (normalize :inline t) ((in vec3) (out vec3)) vec3
  (let ((length (length in)))
    (unless (zerop length)
      (* in (cl:/ length) out)))
  out)

(defspecialization (normalize :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (normalize in (the vec3 (vec3))))

(defspecialization (negate :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:- v.x)
           o.y (cl:- v.y)
           o.z (cl:- v.z)))
  out)

(defspecialization (negate :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (negate in (the vec3 (vec3))))

(defspecialization (lerp :inline t) ((in1 vec3) (in2 vec3) (factor real) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (fl.util:lerp factor v1.x v2.x)
           o.y (fl.util:lerp factor v1.y v2.y)
           o.z (fl.util:lerp factor v1.z v2.z)))
  out)

(defspecialization (lerp :inline t) ((in1 vec3) (in2 vec3) (factor real) (out null)) vec3
  (declare (ignore out))
  (lerp in1 in2 factor (the vec3 (vec3))))

(defspecialization (radians :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (float (fl.util:degrees->radians v.x) 1f0)
           o.y (float (fl.util:degrees->radians v.y) 1f0)
           o.z (float (fl.util:degrees->radians v.z) 1f0)))
  out)

(defspecialization (radians :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (radians in (the vec3 (vec3))))

(defspecialization (degrees :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (float (fl.util:radians->degrees v.x) 1f0)
           o.y (float (fl.util:radians->degrees v.y) 1f0)
           o.z (float (fl.util:radians->degrees v.z) 1f0)))
  out)

(defspecialization (degrees :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (degrees in (the vec3 (vec3))))

(defspecialization (= :inline t) ((in1 vec3) (in2 vec3)) boolean
  (with-vec3 ((v1 in1) (v2 in2))
    (and (cl:= v1.x v2.x)
         (cl:= v1.y v2.y)
         (cl:= v1.z v2.z))))

(defspecialization (~ :inline t) ((in1 vec3) (in2 vec3)) boolean
  (with-vec3 ((v1 in1) (v2 in2))
    (and (cl:< (cl:abs (cl:- v1.x v2.x)) 1e-7)
         (cl:< (cl:abs (cl:- v1.y v2.y)) 1e-7)
         (cl:< (cl:abs (cl:- v1.z v2.z)) 1e-7))))

(defspecialization (< :inline t) ((in1 vec3) (in2 vec3)) boolean
  (with-vec3 ((v1 in1) (v2 in2))
    (and (cl:< v1.x v2.x)
         (cl:< v1.y v2.y)
         (cl:< v1.z v2.z))))

(defspecialization (<= :inline t) ((in1 vec3) (in2 vec3)) boolean
  (with-vec3 ((v1 in1) (v2 in2))
    (and (cl:<= v1.x v2.x)
         (cl:<= v1.y v2.y)
         (cl:<= v1.z v2.z))))

(defspecialization (> :inline t) ((in1 vec3) (in2 vec3)) boolean
  (with-vec3 ((v1 in1) (v2 in2))
    (and (cl:> v1.x v2.x)
         (cl:> v1.y v2.y)
         (cl:> v1.z v2.z))))

(defspecialization (>= :inline t) ((in1 vec3) (in2 vec3)) boolean
  (with-vec3 ((v1 in1) (v2 in2))
    (and (cl:>= v1.x v2.x)
         (cl:>= v1.y v2.y)
         (cl:>= v1.z v2.z))))

(defspecialization (expt :inline t) ((in vec3) (power real) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:expt v.x power)
           o.y (cl:expt v.y power)
           o.z (cl:expt v.z power)))
  out)

(defspecialization (expt :inline t) ((in vec3) (power real) (out null)) vec3
  (declare (ignore out))
  (expt in power (the vec3 (vec3))))

(defspecialization (sqrt :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:sqrt v.x)
           o.y (cl:sqrt v.y)
           o.z (cl:sqrt v.z)))
  out)

(defspecialization (sqrt :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (sqrt in (the vec3 (vec3))))

(defspecialization (floor :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (float (cl:floor v.x) 1f0)
           o.y (float (cl:floor v.y) 1f0)
           o.z (float (cl:floor v.z) 1f0)))
  out)

(defspecialization (floor :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (floor in (the vec3 (vec3))))

(defspecialization (ceiling :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (float (cl:ceiling v.x) 1f0)
           o.y (float (cl:ceiling v.y) 1f0)
           o.z (float (cl:ceiling v.z) 1f0)))
  out)

(defspecialization (ceiling :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (ceiling in (the vec3 (vec3))))

(defspecialization (mod :inline t) ((in vec3) (divisor real) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:mod v.x divisor)
           o.y (cl:mod v.y divisor)
           o.z (cl:mod v.z divisor)))
  out)

(defspecialization (mod :inline t) ((in vec3) (divisor real) (out null)) vec3
  (declare (ignore out))
  (mod in divisor (the vec3 (vec3))))

(defspecialization (round :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (fround v.x)
           o.y (fround v.y)
           o.z (fround v.z)))
  out)

(defspecialization (round :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (round in (the vec3 (vec3))))

(defspecialization (abs :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:abs v.x)
           o.y (cl:abs v.y)
           o.z (cl:abs v.z)))
  out)

(defspecialization (abs :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (abs in (the vec3 (vec3))))

(defspecialization (min :inline t) ((in1 vec3) (in2 vec3) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:min v1.x v2.x)
           o.y (cl:min v1.y v2.y)
           o.z (cl:min v1.z v2.z)))
  out)

(defspecialization (min :inline t) ((in1 vec3) (in2 vec3) (out null)) vec3
  (declare (ignore out))
  (min in1 in2 (the vec3 (vec3))))

(defspecialization (max :inline t) ((in1 vec3) (in2 vec3) (out vec3)) vec3
  (with-vec3 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:max v1.x v2.x)
           o.y (cl:max v1.y v2.y)
           o.z (cl:max v1.z v2.z)))
  out)

(defspecialization (max :inline t) ((in1 vec3) (in2 vec3) (out null)) vec3
  (declare (ignore out))
  (max in1 in2 (the vec3 (vec3))))

(defspecialization (sin :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:sin v.x)
           o.y (cl:sin v.y)
           o.z (cl:sin v.z)))
  out)

(defspecialization (sin :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (sin in (the vec3 (vec3))))

(defspecialization (cos :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:cos v.x)
           o.y (cl:cos v.y)
           o.z (cl:cos v.z)))
  out)

(defspecialization (cos :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (cos in (the vec3 (vec3))))

(defspecialization (tan :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:tan v.x)
           o.y (cl:tan v.y)
           o.z (cl:tan v.z)))
  out)

(defspecialization (tan :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (tan in (the vec3 (vec3))))

(defspecialization (asin :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:asin v.x)
           o.y (cl:asin v.y)
           o.z (cl:asin v.z)))
  out)

(defspecialization (asin :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (asin in (the vec3 (vec3))))

(defspecialization (acos :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:acos v.x)
           o.y (cl:acos v.y)
           o.z (cl:acos v.z)))
  out)

(defspecialization (acos :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (acos in (the vec3 (vec3))))

(defspecialization (atan :inline t) ((in vec3) (out vec3)) vec3
  (with-vec3 ((v in) (o out))
    (psetf o.x (cl:atan v.x)
           o.y (cl:atan v.y)
           o.z (cl:atan v.z)))
  out)

(defspecialization (atan :inline t) ((in vec3) (out null)) vec3
  (declare (ignore out))
  (atan in (the vec3 (vec3))))

(defspecialization (direction= :inline t) ((in1 vec3) (in2 vec3)) boolean
  (cl:>= (dot (normalize in1) (normalize in2))
         (cl:- 1 1e-7)))

(defspecialization (parallel-p :inline t) ((in1 vec3) (in2 vec3)) boolean
  (~ (the vec3 (cross in1 in2)) (the vec3 (vec3))))
