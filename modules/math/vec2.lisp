(in-package :first-light.math)

;;; constants

(fl.util:define-constant +zero-vec2+
    (%vec2 0f0 0f0)
  :test #'equalp)

;;; array accessor

(defspecialization (get-array :inline t) ((vec vec2)) (simple-array single-float (2))
  (v2-array vec))

;;; component accessors

(defspecialization (x :inline t) ((vec vec2)) single-float
  (with-vec2 ((v vec))
    v.x))

(defspecialization ((setf x) :inline t) ((value real) (vec vec2)) single-float
  (with-vec2 ((v vec))
    (setf v.x (float value 1f0))))

(defspecialization (y :inline t) ((vec vec2)) single-float
  (with-vec2 ((v vec))
    v.y))

(defspecialization ((setf y) :inline t) ((value real) (vec vec2)) single-float
  (with-vec2 ((v vec))
    (setf v.y value)))

;;; constructors

(defspecialization (vec2 :inline t) () vec2
  (%vec2 0f0 0f0))

(defspecialization (vec2 :inline t) ((x real)) vec2
  (%vec2 (float x 1f0) (float x 1f0)))

(defspecialization (vec2 :inline t) ((xy vec2)) vec2
  (%vec2 (x xy) (y xy)))

(defspecialization (vec2 :inline t) ((xyz vec3)) vec2
  (%vec2 (x xyz) (y xyz)))

(defspecialization (vec2 :inline t) ((xyzw vec4)) vec2
  (%vec2 (x xyzw) (y xyzw)))

(defspecialization (vec2 :inline t) ((x real) (y real)) vec2
  (%vec2 (float x 1f0) (float y 1f0)))

;;; copiers

(defspecialization (copy :inline t) ((in vec2)) vec2
  (%vec2 (x in) (y in)))

(defspecialization (copy :inline t) ((in vec2) (x real)) vec2
  (%vec2 (float x 1f0) (y in)))

(defspecialization (copy :inline t) ((in vec2) (xy vec2)) vec2
  (declare (ignore in))
  (%vec2 (x xy) (y xy)))

(defspecialization (copy :inline t) ((in vec2) (xyz vec3)) vec2
  (declare (ignore in))
  (%vec2 (x xyz) (y xyz)))

(defspecialization (copy :inline t) ((in vec2) (xyzw vec4)) vec2
  (declare (ignore in))
  (%vec2 (x xyzw) (y xyzw)))

(defspecialization (copy :inline t) ((in vec2) (x real) (y real)) vec2
  (declare (ignore in))
  (%vec2 (float x 1f0) (float y 1f0)))

(defspecialization (copy-into :inline t) ((out vec2)) vec2
  out)

(defspecialization (copy-into :inline t) ((out vec2) (x real)) vec2
  (with-vec2 ((o out))
    (setf o.x (float x 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out vec2) (xy vec2)) vec2
  (with-vec2 ((o out) (v xy))
    (setf o.x v.x o.y v.y))
  out)

(defspecialization (copy-into :inline t) ((out vec2) (xyz vec3)) vec2
  (with-vec2 ((o out))
    (with-vec3 ((v xyz))
      (setf o.x v.x o.y v.y)))
  out)

(defspecialization (copy-into :inline t) ((out vec2) (xyzw vec4)) vec2
  (with-vec2 ((o out))
    (with-vec4 ((v xyzw))
      (setf o.x v.x o.y v.y)))
  out)

(defspecialization (copy-into :inline t) ((out vec2) (x real) (y real)) vec2
  (with-vec2 ((o out))
    (setf o.x (float x 1f0) o.y (float y 1f0)))
  out)

;;; operations

(defspecialization (rand :inline t) ((in vec2) (min real) (max real) (out vec2)) vec2
  (declare (ignore in))
  (let ((min (float min 1f0))
        (max (float max 1f0)))
    (with-vec2 ((o out))
      (psetf o.x (cl:+ min (random (cl:- max min)))
             o.y (cl:+ min (random (cl:- max min))))))
  out)

(defspecialization (rand :inline t) ((in vec2) (min real) (max real) (out null)) vec2
  (declare (ignore out))
  (rand in min max (the vec2 (vec2))))

(defspecialization (zero :inline t) ((out vec2)) vec2
  (with-vec2 ((o out))
    (psetf o.x 0f0 o.y 0f0))
  out)

(defspecialization (zero-p :inline t) ((in vec2)) boolean
  (with-vec2 ((v in))
    (cl:= 0f0 v.x v.y)))

(defspecialization (unit-p :inline t) ((in vec2)) boolean
  (with-vec2 ((v in))
    (cl:= 0f0 (cl:- 1f0 (cl:+ (cl:expt v.x 2) (cl:expt v.y 2))))))

(defspecialization (clamp :inline t) ((in vec2) (min real) (max real) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (float (fl.util:clamp v.x min max) 1f0)
           o.y (float (fl.util:clamp v.y min max) 1f0)))
  out)

(defspecialization (clamp :inline t) ((in vec2) (min real) (max real) (out null)) vec2
  (declare (ignore out))
  (clamp in min max (the vec2 (vec2))))

(defspecialization (stabilize :inline t) ((in vec2) (tolerance single-float) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (macrolet ((%stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0f0 ,place)))
      (psetf o.x (%stabilize v.x)
             o.y (%stabilize v.y))))
  out)

(defspecialization (stabilize :inline t) ((in vec2) (tolerance single-float) (out null)) vec2
  (declare (ignore out))
  (stabilize in tolerance (the vec2 (vec2))))

(defspecialization (+ :inline t) ((in1 vec2) (in2 vec2) (out vec2)) vec2
  (with-vec2 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:+ v1.x v2.x)
           o.y (cl:+ v1.y v2.y)))
  out)

(defspecialization (+ :inline t) ((in1 vec2) (in2 vec2) (out null)) vec2
  (declare (ignore out))
  (+ in1 in2 (the vec2 (vec2))))

(defspecialization (+ :inline t) ((in1 vec2) (in2 real) (out vec2)) vec2
  (with-vec2 ((v in1) (o out))
    (psetf o.x (cl:+ v.x in2)
           o.y (cl:+ v.y in2)))
  out)

(defspecialization (+ :inline t) ((in1 vec2) (in2 real) (out null)) vec2
  (declare (ignore out))
  (+ in1 in2 (the vec2 (vec2))))

(defspecialization (- :inline t) ((in1 vec2) (in2 vec2) (out vec2)) vec2
  (with-vec2 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:- v1.x v2.x)
           o.y (cl:- v1.y v2.y)))
  out)

(defspecialization (- :inline t) ((in1 vec2) (in2 vec2) (out null)) vec2
  (declare (ignore out))
  (- in1 in2 (the vec2 (vec2))))

(defspecialization (- :inline t) ((in1 vec2) (in2 real) (out vec2)) vec2
  (with-vec2 ((v in1) (o out))
    (psetf o.x (cl:- v.x in2)
           o.y (cl:- v.y in2)))
  out)

(defspecialization (- :inline t) ((in1 vec2) (in2 real) (out null)) vec2
  (declare (ignore out))
  (- in1 in2 (the vec2 (vec2))))

(defspecialization (* :inline t) ((in1 vec2) (in2 vec2) (out vec2)) vec2
  (with-vec2 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:* v1.x v2.x)
           o.y (cl:* v1.y v2.y)))
  out)

(defspecialization (* :inline t) ((in1 vec2) (in2 vec2) (out null)) vec2
  (declare (ignore out))
  (* in1 in2 (the vec2 (vec2))))

(defspecialization (* :inline t) ((in1 vec2) (in2 real) (out vec2)) vec2
  (with-vec2 ((v in1) (o out))
    (psetf o.x (cl:* v.x in2)
           o.y (cl:* v.y in2)))
  out)

(defspecialization (* :inline t) ((in1 vec2) (in2 real) (out null)) vec2
  (declare (ignore out))
  (* in1 in2 (the vec2 (vec2))))

(defspecialization (/ :inline t) ((in1 vec2) (in2 vec2) (out vec2)) vec2
  (with-vec2 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (if (zerop v2.x) 0f0 (cl:/ v1.x v2.x))
           o.y (if (zerop v2.y) 0f0 (cl:/ v1.y v2.y))))
  out)

(defspecialization (/ :inline t) ((in1 vec2) (in2 vec2) (out null)) vec2
  (declare (ignore out))
  (/ in1 in2 (the vec2 (vec2))))

(defspecialization (/ :inline t) ((in1 vec2) (in2 real) (out vec2)) vec2
  (with-vec2 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (if (zerop in2) 0f0 (cl:/ v1.x in2))
           o.y (if (zerop in2) 0f0 (cl:/ v1.y in2))))
  out)

(defspecialization (/ :inline t) ((in1 vec2) (in2 real) (out null)) vec2
  (declare (ignore out))
  (/ in1 in2 (the vec2 (vec2))))

(defspecialization (sign :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (float (signum v.x) 1f0)
           o.y (float (signum v.y) 1f0)))
  out)

(defspecialization (sign :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (sign in (the vec2 (vec2))))

(defspecialization (fract :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (nth-value 1 (cl:floor v.x))
           o.y (nth-value 1 (cl:floor v.y))))
  out)

(defspecialization (fract :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (fract in (the vec2 (vec2))))

(defspecialization (dot :inline t) ((in1 vec2) (in2 vec2)) single-float
  (with-vec2 ((v1 in1) (v2 in2))
    (cl:+ (cl:* v1.x v2.x)
          (cl:* v1.y v2.y))))

(defspecialization (length-squared :inline t) ((in vec2)) single-float
  (dot in in))

(defspecialization (length :inline t) ((in vec2)) single-float
  (cl:sqrt (length-squared in)))

(defspecialization (distance-squared :inline t) ((in1 vec2) (in2 vec2)) single-float
  (length-squared (the vec2 (- in2 in1))))

(defspecialization (normalize :inline t) ((in vec2) (out vec2)) vec2
  (let ((length (length in)))
    (unless (zerop length)
      (* in (cl:/ length) out)))
  out)

(defspecialization (normalize :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (normalize in (the vec2 (vec2))))

(defspecialization (negate :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:- v.x)
           o.y (cl:- v.y)))
  out)

(defspecialization (negate :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (negate in (the vec2 (vec2))))

(defspecialization (lerp :inline t) ((in1 vec2) (in2 vec2) (factor real) (out vec2)) vec2
  (with-vec2 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (fl.util:lerp factor v1.x v2.x)
           o.y (fl.util:lerp factor v1.y v2.y)))
  out)

(defspecialization (lerp :inline t) ((in1 vec2) (in2 vec2) (factor real) (out null)) vec2
  (declare (ignore out))
  (lerp in1 in2 factor (the vec2 (vec2))))

(defspecialization (radians :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (float (fl.util:degrees->radians v.x) 1f0)
           o.y (float (fl.util:degrees->radians v.y) 1f0)))
  out)

(defspecialization (radians :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (radians in (the vec2 (vec2))))

(defspecialization (degrees :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (float (fl.util:radians->degrees v.x) 1f0)
           o.y (float (fl.util:radians->degrees v.y) 1f0)))
  out)

(defspecialization (degrees :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (degrees in (the vec2 (vec2))))

(defspecialization (direction= :inline t) ((in1 vec2) (in2 vec2)) boolean
  (cl:>= (dot (normalize in1) (normalize in2))
         (cl:- 1 1e-7)))

(defspecialization (= :inline t) ((in1 vec2) (in2 vec2)) boolean
  (with-vec2 ((v1 in1) (v2 in2))
    (and (cl:= v1.x v2.x)
         (cl:= v1.y v2.y))))

(defspecialization (~ :inline t) ((in1 vec2) (in2 vec2)) boolean
  (with-vec2 ((v1 in1) (v2 in2))
    (and (cl:< (cl:abs (cl:- v1.x v2.x)) 1e-7)
         (cl:< (cl:abs (cl:- v1.y v2.y)) 1e-7))))

(defspecialization (< :inline t) ((in1 vec2) (in2 vec2)) boolean
  (with-vec2 ((v1 in1) (v2 in2))
    (and (cl:< v1.x v2.x)
         (cl:< v1.y v2.y))))

(defspecialization (<= :inline t) ((in1 vec2) (in2 vec2)) boolean
  (with-vec2 ((v1 in1) (v2 in2))
    (and (cl:<= v1.x v2.x)
         (cl:<= v1.y v2.y))))

(defspecialization (> :inline t) ((in1 vec2) (in2 vec2)) boolean
  (with-vec2 ((v1 in1) (v2 in2))
    (and (cl:> v1.x v2.x)
         (cl:> v1.y v2.y))))

(defspecialization (>= :inline t) ((in1 vec2) (in2 vec2)) boolean
  (with-vec2 ((v1 in1) (v2 in2))
    (and (cl:>= v1.x v2.x)
         (cl:>= v1.y v2.y))))

(defspecialization (expt :inline t) ((in vec2) (power real) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:expt v.x power)
           o.y (cl:expt v.y power)))
  out)

(defspecialization (expt :inline t) ((in vec2) (power real) (out null)) vec2
  (declare (ignore out))
  (expt in power (the vec2 (vec2))))

(defspecialization (sqrt :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:sqrt v.x)
           o.y (cl:sqrt v.y)))
  out)

(defspecialization (sqrt :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (sqrt in (the vec2 (vec2))))

(defspecialization (floor :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (float (cl:floor v.x) 1f0)
           o.y (float (cl:floor v.y) 1f0)))
  out)

(defspecialization (floor :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (floor in (the vec2 (vec2))))

(defspecialization (ceiling :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (float (cl:ceiling v.x) 1f0)
           o.y (float (cl:ceiling v.y) 1f0)))
  out)

(defspecialization (ceiling :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (ceiling in (the vec2 (vec2))))

(defspecialization (mod :inline t) ((in vec2) (divisor real) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:mod v.x divisor)
           o.y (cl:mod v.y divisor)))
  out)

(defspecialization (mod :inline t) ((in vec2) (divisor real) (out null)) vec2
  (declare (ignore out))
  (mod in divisor (the vec2 (vec2))))

(defspecialization (round :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (fround v.x)
           o.y (fround v.y)))
  out)

(defspecialization (round :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (round in (the vec2 (vec2))))

(defspecialization (abs :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:abs v.x)
           o.y (cl:abs v.y)))
  out)

(defspecialization (abs :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (abs in (the vec2 (vec2))))

(defspecialization (min :inline t) ((in1 vec2) (in2 vec2) (out vec2)) vec2
  (with-vec2 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:min v1.x v2.x)
           o.y (cl:min v1.y v2.y)))
  out)

(defspecialization (min :inline t) ((in1 vec2) (in2 vec2) (out null)) vec2
  (declare (ignore out))
  (min in1 in2 (the vec2 (vec2))))

(defspecialization (max :inline t) ((in1 vec2) (in2 vec2) (out vec2)) vec2
  (with-vec2 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:max v1.x v2.x)
           o.y (cl:max v1.y v2.y)))
  out)

(defspecialization (max :inline t) ((in1 vec2) (in2 vec2) (out null)) vec2
  (declare (ignore out))
  (max in1 in2 (the vec2 (vec2))))

(defspecialization (sin :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:sin v.x)
           o.y (cl:sin v.y)))
  out)

(defspecialization (sin :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (sin in (the vec2 (vec2))))

(defspecialization (cos :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:cos v.x)
           o.y (cl:cos v.y)))
  out)

(defspecialization (cos :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (cos in (the vec2 (vec2))))

(defspecialization (tan :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:tan v.x)
           o.y (cl:tan v.y)))
  out)

(defspecialization (tan :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (tan in (the vec2 (vec2))))

(defspecialization (asin :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:asin v.x)
           o.y (cl:asin v.y)))
  out)

(defspecialization (asin :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (asin in (the vec2 (vec2))))

(defspecialization (acos :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:acos v.x)
           o.y (cl:acos v.y)))
  out)

(defspecialization (acos :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (acos in (the vec2 (vec2))))

(defspecialization (atan :inline t) ((in vec2) (out vec2)) vec2
  (with-vec2 ((v in) (o out))
    (psetf o.x (cl:atan v.x)
           o.y (cl:atan v.y)))
  out)

(defspecialization (atan :inline t) ((in vec2) (out null)) vec2
  (declare (ignore out))
  (atan in (the vec2 (vec2))))
