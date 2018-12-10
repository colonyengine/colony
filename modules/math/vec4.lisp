(in-package :fl.math)

;;; constants

(fu:define-constant +zero-vec4+
    (%vec4 0f0 0f0 0f0 0f0)
  :test #'equalp)

;;; array accessor

(defspecialization (get-array :inline t) ((vec vec4)) (simple-array single-float (4))
  (v4-array vec))

;;; component accessors

(defspecialization (x :inline t) ((vec vec4)) single-float
  (with-vec4 ((v vec))
    v.x))

(defspecialization ((setf x) :inline t) ((value real) (vec vec4)) single-float
  (with-vec4 ((v vec))
    (setf v.x (float value 1f0))))

(defspecialization (y :inline t) ((vec vec4)) single-float
  (with-vec4 ((v vec))
    v.y))

(defspecialization ((setf y) :inline t) ((value real) (vec vec4)) single-float
  (with-vec4 ((v vec))
    (setf v.y (float value 1f0))))

(defspecialization (z :inline t) ((vec vec4)) single-float
  (with-vec4 ((v vec))
    v.z))

(defspecialization ((setf z) :inline t) ((value real) (vec vec4)) single-float
  (with-vec4 ((v vec))
    (setf v.z (float value 1f0))))

(defspecialization (w :inline t) ((vec vec4)) single-float
  (with-vec4 ((v vec))
    v.w))

(defspecialization ((setf w) :inline t) ((value real) (vec vec4)) single-float
  (with-vec4 ((v vec))
    (setf v.w (float value 1f0))))

;; constructors

(defspecialization (vec4 :inline t) () vec4
  (%vec4 0f0 0f0 0f0 0f0))

(defspecialization (vec4 :inline t) ((x real)) vec4
  (%vec4 (float x 1f0) (float x 1f0) (float x 1f0) (float x 1f0)))

(defspecialization (vec4 :inline t) ((xy vec2)) vec4
  (%vec4 (x xy) (y xy) 0f0 0f0))

(defspecialization (vec4 :inline t) ((xyz vec3)) vec4
  (%vec4 (x xyz) (y xyz) (z xyz) 0f0))

(defspecialization (vec4 :inline t) ((xyzw vec4)) vec4
  (%vec4 (x xyzw) (y xyzw) (z xyzw) (w xyzw)))

(defspecialization (vec4 :inline t) ((x real) (y real)) vec4
  (%vec4 (float x 1f0) (float y 1f0) 0f0 0f0))

(defspecialization (vec4 :inline t) ((xy vec2) (z real)) vec4
  (%vec4 (x xy) (y xy) (float z 1f0) 0f0))

(defspecialization (vec4 :inline t) ((x real) (yz vec2)) vec4
  (%vec4 (float x 1f0) (x yz) (y yz) 0f0))

(defspecialization (vec4 :inline t) ((xy vec2) (yz vec2)) vec4
  (%vec4 (x xy) (y xy) (x yz) (y yz)))

(defspecialization (vec4 :inline t) ((x real) (yzw vec3)) vec4
  (%vec4 (float x 1f0) (x yzw) (y yzw) (z yzw)))

(defspecialization (vec4 :inline t) ((xyz vec3) (w real)) vec4
  (%vec4 (x xyz) (y xyz) (z xyz) (float w 1f0)))

(defspecialization (vec4 :inline t) ((x real) (y real) (z real)) vec4
  (%vec4 (float x 1f0) (float y 1f0) (float z 1f0) 0f0))

(defspecialization (vec4 :inline t) ((xy vec2) (z real) (w real)) vec4
  (%vec4 (x xy) (y xy) (float z 1f0) (float w 1f0)))

(defspecialization (vec4 :inline t) ((x real) (y real) (zw vec2)) vec4
  (%vec4 (float x 1f0) (float y 1f0) (x zw) (y zw)))

(defspecialization (vec4 :inline t) ((x real) (yz vec2) (w real)) vec4
  (%vec4 (float x 1f0) (x yz) (y yz) (float w 1f0)))

(defspecialization (vec4 :inline t) ((x real) (y real) (z real) (w real)) vec4
  (%vec4 (float x 1f0) (float y 1f0) (float z 1f0) (float w 1f0)))

(defspecialization (vec4 :inline t) ((wxyz quat)) vec4
  (%vec4 (w wxyz) (x wxyz) (y wxyz) (z wxyz)))

;;; copiers

(defspecialization (copy :inline t) ((in vec4)) vec4
  (%vec4 (x in) (y in) (z in) (w in)))

(defspecialization (copy :inline t) ((in vec4) (x real)) vec4
  (%vec4 (float x 1f0) (y in) (z in) (w in)))

(defspecialization (copy :inline t) ((in vec4) (xy vec2)) vec4
  (%vec4 (x xy) (y xy) (z in) (w in)))

(defspecialization (copy :inline t) ((in vec4) (xyz vec3)) vec4
  (%vec4 (x xyz) (y xyz) (z xyz) (w in)))

(defspecialization (copy :inline t) ((in vec4) (xyzw vec4)) vec4
  (declare (ignore in))
  (%vec4 (x xyzw) (y xyzw) (z xyzw) (w xyzw)))

(defspecialization (copy :inline t) ((in vec4) (x real) (y real)) vec4
  (%vec4 (float x 1f0) (float y 1f0) (z in) (w in)))

(defspecialization (copy :inline t) ((in vec4) (xy vec2) (z real)) vec4
  (%vec4 (x xy) (y xy) (float z 1f0) (w in)))

(defspecialization (copy :inline t) ((in vec4) (x real) (yz vec2)) vec4
  (%vec4 (float x 1f0) (x yz) (y yz) (w in)))

(defspecialization (copy :inline t) ((in vec4) (xy vec2) (zw vec2)) vec4
  (declare (ignore in))
  (%vec4 (x xy) (y xy) (x zw) (y zw)))

(defspecialization (copy :inline t) ((in vec4) (x real) (yzw vec3)) vec4
  (declare (ignore in))
  (%vec4 (float x 1f0) (x yzw) (y yzw) (z yzw)))

(defspecialization (copy :inline t) ((in vec4) (xyz vec3) (w real)) vec4
  (declare (ignore in))
  (%vec4 (x xyz) (y xyz) (z xyz) (float w 1f0)))

(defspecialization (copy :inline t) ((in vec4) (x real) (y real) (z real)) vec4
  (%vec4 (float x 1f0) (float y 1f0) (float z 1f0) (w in)))

(defspecialization (copy :inline t) ((in vec4) (xy vec2) (z real) (w real)) vec4
  (declare (ignore in))
  (%vec4 (x xy) (y xy) (float z 1f0) (float w 1f0)))

(defspecialization (copy :inline t) ((in vec4) (x real) (y real) (zw vec2)) vec4
  (declare (ignore in))
  (%vec4 (float x 1f0) (float y 1f0) (x zw) (y zw)))

(defspecialization (copy :inline t) ((in vec4) (x real) (yz vec2) (w real)) vec4
  (declare (ignore in))
  (%vec4 (float x 1f0) (x yz) (y yz) (float w 1f0)))

(defspecialization (copy :inline t) ((in vec4) (x real) (y real) (z real) (w real)) vec4
  (declare (ignore in))
  (%vec4 (float x 1f0) (float y 1f0) (float z 1f0) (float w 1f0)))

(defspecialization (copy-into :inline t) ((out vec4)) vec4
  out)

(defspecialization (copy-into :inline t) ((out vec4) (x real)) vec4
  (with-vec4 ((o out))
    (setf o.x (float x 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (xy vec2)) vec4
  (with-vec4 ((o out))
    (with-vec2 ((v xy))
      (setf o.x v.x o.y v.y)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (xyz vec3)) vec4
  (with-vec4 ((o out))
    (with-vec3 ((v xyz))
      (setf o.x v.x o.y v.y o.z v.z)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (xyzw vec4)) vec4
  (with-vec4 ((o out) (v xyzw))
    (setf o.x v.x o.y v.y o.z v.z o.w v.w))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (x real) (y real)) vec4
  (with-vec4 ((o out))
    (setf o.x (float x 1f0) o.y (float y 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (xy vec2) (y real)) vec4
  (with-vec4 ((o out))
    (with-vec2 ((v xy))
      (setf o.x v.x o.y v.y o.z (float y 1f0))))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (x real) (yz vec2)) vec4
  (with-vec4 ((o out))
    (with-vec2 ((v yz))
      (setf o.x (float x 1f0) o.y v.x o.z v.y)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (xy vec2) (zw vec2)) vec4
  (with-vec4 ((o out))
    (with-vec2 ((v1 xy) (v2 zw))
      (setf o.x v1.x o.y v1.y o.z v2.x o.w v2.y)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (x real) (yzw vec3)) vec4
  (with-vec4 ((o out))
    (with-vec3 ((v yzw))
      (setf o.x (float x 1f0) o.y v.x o.z v.y o.w v.z)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (xyz vec3) (w real)) vec4
  (with-vec4 ((o out))
    (with-vec3 ((v xyz))
      (setf o.x v.x o.y v.y o.z v.z o.w (float w 1f0))))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (x real) (y real) (z real)) vec4
  (with-vec4 ((o out))
    (setf o.x (float x 1f0) o.y (float y 1f0) o.z (float z 1f0)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (xy vec2) (z real) (w real)) vec4
  (with-vec4 ((o out))
    (with-vec2 ((v xy))
      (setf o.x v.x o.y v.y o.z (float z 1f0) o.w (float w 1f0))))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (x real) (y real) (zw vec2)) vec4
  (with-vec4 ((o out))
    (with-vec2 ((v zw))
      (setf o.x (float x 1f0) o.y (float y 1f0) o.z v.x o.w v.y)))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (x real) (yz vec2) (w real)) vec4
  (with-vec4 ((o out))
    (with-vec2 ((v yz))
      (setf o.x (float x 1f0) o.y v.x o.z v.y o.w (float w 1f0))))
  out)

(defspecialization (copy-into :inline t) ((out vec4) (x real) (y real) (z real) (w real)) vec4
  (with-vec4 ((o out))
    (setf o.x (float x 1f0) o.y (float y 1f0) o.z (float z 1f0) o.w (float w 1f0)))
  out)

;;; operations

(defspecialization (rand :inline t) ((in vec4) (min real) (max real) (out vec4)) vec4
  (declare (ignore in))
  (let ((min (float min 1f0))
        (max (float max 1f0)))
    (with-vec4 ((o out))
      (psetf o.x (cl:+ min (random (cl:- max min)))
             o.y (cl:+ min (random (cl:- max min)))
             o.z (cl:+ min (random (cl:- max min)))
             o.w (cl:+ min (random (cl:- max min))))))
  out)

(defspecialization (rand :inline t) ((in vec4) (min real) (max real) (out null)) vec4
  (declare (ignore out))
  (rand in min max (the vec4 (vec4))))

(defspecialization (zero :inline t) ((out vec4)) vec4
  (with-vec4 ((o out))
    (psetf o.x 0f0 o.y 0f0 o.z 0f0 o.w 0f0))
  out)

(defspecialization (zero-p :inline t) ((in vec4)) boolean
  (with-vec4 ((v in))
    (cl:= 0f0 v.x v.y v.z v.w)))

(defspecialization (unit-p :inline t) ((in vec4)) boolean
  (with-vec4 ((v in))
    (cl:= 0f0 (cl:- 1f0 (cl:+ (cl:expt v.x 2) (cl:expt v.y 2) (cl:expt v.z 2) (cl:expt v.w 2))))))

(defspecialization (clamp :inline t) ((in vec4) (min real) (max real) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (float (fu:clamp v.x min max) 1f0)
           o.y (float (fu:clamp v.y min max) 1f0)
           o.z (float (fu:clamp v.z min max) 1f0)
           o.w (float (fu:clamp v.w min max) 1f0)))
  out)

(defspecialization (clamp :inline t) ((in vec4) (min real) (max real) (out null)) vec4
  (declare (ignore out))
  (clamp in min max (the vec4 (vec4))))

(defspecialization (stabilize :inline t) ((in vec4) (tolerance single-float) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (macrolet ((%stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0f0 ,place)))
      (psetf o.x (%stabilize v.x)
             o.y (%stabilize v.y)
             o.z (%stabilize v.z)
             o.w (%stabilize v.w))))
  out)

(defspecialization (stabilize :inline t) ((in vec4) (tolerance single-float) (out null)) vec4
  (declare (ignore out))
  (stabilize in tolerance (the vec4 (vec4))))

(defspecialization (+ :inline t) ((in1 vec4) (in2 vec4) (out vec4)) vec4
  (with-vec4 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:+ v1.x v2.x)
           o.y (cl:+ v1.y v2.y)
           o.z (cl:+ v1.z v2.z)
           o.w (cl:+ v1.w v2.w)))
  out)

(defspecialization (+ :inline t) ((in1 vec4) (in2 vec4) (out null)) vec4
  (declare (ignore out))
  (+ in1 in2 (the vec4 (vec4))))

(defspecialization (+ :inline t) ((in1 vec4) (in2 real) (out vec4)) vec4
  (with-vec4 ((v in1) (o out))
    (psetf o.x (cl:+ v.x in2)
           o.y (cl:+ v.y in2)
           o.z (cl:+ v.z in2)
           o.w (cl:+ v.w in2)))
  out)

(defspecialization (+ :inline t) ((in1 vec4) (in2 real) (out null)) vec4
  (declare (ignore out))
  (+ in1 in2 (the vec4 (vec4))))

(defspecialization (- :inline t) ((in1 vec4) (in2 vec4) (out vec4)) vec4
  (with-vec4 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:- v1.x v2.x)
           o.y (cl:- v1.y v2.y)
           o.z (cl:- v1.z v2.z)
           o.w (cl:- v1.w v2.w)))
  out)

(defspecialization (- :inline t) ((in1 vec4) (in2 vec4) (out null)) vec4
  (declare (ignore out))
  (- in1 in2 (the vec4 (vec4))))

(defspecialization (- :inline t) ((in1 vec4) (in2 real) (out vec4)) vec4
  (with-vec4 ((v in1) (o out))
    (psetf o.x (cl:- v.x in2)
           o.y (cl:- v.y in2)
           o.z (cl:- v.z in2)
           o.w (cl:- v.w in2)))
  out)

(defspecialization (- :inline t) ((in1 vec4) (in2 real) (out null)) vec4
  (declare (ignore out))
  (- in1 in2 (the vec4 (vec4))))

(defspecialization (* :inline t) ((in1 vec4) (in2 vec4) (out vec4)) vec4
  (with-vec4 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:* v1.x v2.x)
           o.y (cl:* v1.y v2.y)
           o.z (cl:* v1.z v2.z)
           o.w (cl:* v1.w v2.w)))
  out)

(defspecialization (* :inline t) ((in1 vec4) (in2 vec4) (out null)) vec4
  (declare (ignore out))
  (* in1 in2 (the vec4 (vec4))))

(defspecialization (* :inline t) ((in1 vec4) (in2 real) (out vec4)) vec4
  (with-vec4 ((v in1) (o out))
    (psetf o.x (cl:* v.x in2)
           o.y (cl:* v.y in2)
           o.z (cl:* v.z in2)
           o.w (cl:* v.w in2)))
  out)

(defspecialization (* :inline t) ((in1 vec4) (in2 real) (out null)) vec4
  (declare (ignore out))
  (* in1 in2 (the vec4 (vec4))))

(defspecialization (/ :inline t) ((in1 vec4) (in2 vec4) (out vec4)) vec4
  (with-vec4 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (if (zerop v2.x) 0f0 (cl:/ v1.x v2.x))
           o.y (if (zerop v2.y) 0f0 (cl:/ v1.y v2.y))
           o.z (if (zerop v2.z) 0f0 (cl:/ v1.z v2.z))
           o.w (if (zerop v2.w) 0f0 (cl:/ v1.w v2.w))))
  out)

(defspecialization (/ :inline t) ((in1 vec4) (in2 vec4) (out null)) vec4
  (declare (ignore out))
  (/ in1 in2 (the vec4 (vec4))))

(defspecialization (/ :inline t) ((in1 vec4) (in2 real) (out vec4)) vec4
  (with-vec4 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (if (zerop in2) 0f0 (cl:/ v1.x in2))
           o.y (if (zerop in2) 0f0 (cl:/ v1.y in2))
           o.z (if (zerop in2) 0f0 (cl:/ v1.z in2))
           o.w (if (zerop in2) 0f0 (cl:/ v1.w in2))))
  out)

(defspecialization (/ :inline t) ((in1 vec4) (in2 real) (out null)) vec4
  (declare (ignore out))
  (/ in1 in2 (the vec4 (vec4))))

(defspecialization (sign :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (float (signum v.x) 1f0)
           o.y (float (signum v.y) 1f0)
           o.z (float (signum v.z) 1f0)
           o.w (float (signum v.w) 1f0)))
  out)

(defspecialization (sign :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (sign in (the vec4 (vec4))))

(defspecialization (fract :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (nth-value 1 (cl:floor v.x))
           o.y (nth-value 1 (cl:floor v.y))
           o.z (nth-value 1 (cl:floor v.z))
           o.w (nth-value 1 (cl:floor v.w))))
  out)

(defspecialization (fract :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (fract in (the vec4 (vec4))))

(defspecialization (dot :inline t) ((in1 vec4) (in2 vec4)) single-float
  (with-vec4 ((v1 in1) (v2 in2))
    (cl:+ (cl:* v1.x v2.x)
          (cl:* v1.y v2.y)
          (cl:* v1.z v2.z)
          (cl:* v1.w v2.w))))

(defspecialization (length-squared :inline t) ((in vec4)) single-float
  (dot in in))

(defspecialization (length :inline t) ((in vec4)) single-float
  (cl:sqrt (length-squared in)))

(defspecialization (distance-squared :inline t) ((in1 vec4) (in2 vec4)) single-float
  (length-squared (the vec4 (- in2 in1))))

(defspecialization (normalize :inline t) ((in vec4) (out vec4)) vec4
  (let ((length (length in)))
    (unless (zerop length)
      (* in (cl:/ length) out)))
  out)

(defspecialization (normalize :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (normalize in (the vec4 (vec4))))

(defspecialization (negate :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:- v.x)
           o.y (cl:- v.y)
           o.z (cl:- v.z)
           o.w (cl:- v.w)))
  out)

(defspecialization (negate :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (negate in (the vec4 (vec4))))

(defspecialization (lerp :inline t) ((in1 vec4) (in2 vec4) (factor real) (out vec4)) vec4
  (with-vec4 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (fu:lerp factor v1.x v2.x)
           o.y (fu:lerp factor v1.y v2.y)
           o.z (fu:lerp factor v1.z v2.z)
           o.w (fu:lerp factor v1.w v2.w)))
  out)

(defspecialization (lerp :inline t) ((in1 vec4) (in2 vec4) (factor real) (out null)) vec4
  (declare (ignore out))
  (lerp in1 in2 factor (the vec4 (vec4))))

(defspecialization (radians :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (float (fu:degrees->radians v.x) 1f0)
           o.y (float (fu:degrees->radians v.y) 1f0)
           o.z (float (fu:degrees->radians v.z) 1f0)
           o.w (float (fu:degrees->radians v.w) 1f0)))
  out)

(defspecialization (radians :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (radians in (the vec4 (vec4))))

(defspecialization (degrees :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (float (fu:radians->degrees v.x) 1f0)
           o.y (float (fu:radians->degrees v.y) 1f0)
           o.z (float (fu:radians->degrees v.z) 1f0)
           o.w (float (fu:radians->degrees v.w) 1f0)))
  out)

(defspecialization (degrees :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (degrees in (the vec4 (vec4))))

(defspecialization (= :inline t) ((in1 vec4) (in2 vec4)) boolean
  (with-vec4 ((v1 in1) (v2 in2))
    (and (cl:= v1.x v2.x)
         (cl:= v1.y v2.y)
         (cl:= v1.z v2.z)
         (cl:= v1.w v2.w))))

(defspecialization (~ :inline t) ((in1 vec4) (in2 vec4)) boolean
  (with-vec4 ((v1 in1) (v2 in2))
    (and (cl:< (cl:abs (cl:- v1.x v2.x)) 1e-7)
         (cl:< (cl:abs (cl:- v1.y v2.y)) 1e-7)
         (cl:< (cl:abs (cl:- v1.z v2.z)) 1e-7)
         (cl:< (cl:abs (cl:- v1.w v2.w)) 1e-7))))

(defspecialization (< :inline t) ((in1 vec4) (in2 vec4)) boolean
  (with-vec4 ((v1 in1) (v2 in2))
    (and (cl:< v1.x v2.x)
         (cl:< v1.y v2.y)
         (cl:< v1.z v2.z)
         (cl:< v1.w v2.w))))

(defspecialization (<= :inline t) ((in1 vec4) (in2 vec4)) boolean
  (with-vec4 ((v1 in1) (v2 in2))
    (and (cl:<= v1.x v2.x)
         (cl:<= v1.y v2.y)
         (cl:<= v1.z v2.z)
         (cl:<= v1.w v2.w))))

(defspecialization (> :inline t) ((in1 vec4) (in2 vec4)) boolean
  (with-vec4 ((v1 in1) (v2 in2))
    (and (cl:> v1.x v2.x)
         (cl:> v1.y v2.y)
         (cl:> v1.z v2.z)
         (cl:> v1.w v2.w))))

(defspecialization (>= :inline t) ((in1 vec4) (in2 vec4)) boolean
  (with-vec4 ((v1 in1) (v2 in2))
    (and (cl:>= v1.x v2.x)
         (cl:>= v1.y v2.y)
         (cl:>= v1.z v2.z)
         (cl:>= v1.w v2.w))))

(defspecialization (expt :inline t) ((in vec4) (power real) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:expt v.x power)
           o.y (cl:expt v.y power)
           o.z (cl:expt v.z power)
           o.w (cl:expt v.w power)))
  out)

(defspecialization (expt :inline t) ((in vec4) (power real) (out null)) vec4
  (declare (ignore out))
  (expt in power (the vec4 (vec4))))

(defspecialization (sqrt :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:sqrt v.x)
           o.y (cl:sqrt v.y)
           o.z (cl:sqrt v.z)
           o.w (cl:sqrt v.w)))
  out)

(defspecialization (sqrt :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (sqrt in (the vec4 (vec4))))

(defspecialization (floor :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (float (cl:floor v.x) 1f0)
           o.y (float (cl:floor v.y) 1f0)
           o.z (float (cl:floor v.z) 1f0)
           o.w (float (cl:floor v.w) 1f0)))
  out)

(defspecialization (floor :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (floor in (the vec4 (vec4))))

(defspecialization (ceiling :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (float (cl:ceiling v.x) 1f0)
           o.y (float (cl:ceiling v.y) 1f0)
           o.z (float (cl:ceiling v.z) 1f0)
           o.w (float (cl:ceiling v.w) 1f0)))
  out)

(defspecialization (ceiling :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (ceiling in (the vec4 (vec4))))

(defspecialization (mod :inline t) ((in vec4) (divisor real) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:mod v.x divisor)
           o.y (cl:mod v.y divisor)
           o.z (cl:mod v.z divisor)
           o.w (cl:mod v.w divisor)))
  out)

(defspecialization (mod :inline t) ((in vec4) (divisor real) (out null)) vec4
  (declare (ignore out))
  (mod in divisor (the vec4 (vec4))))

(defspecialization (round :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (fround v.x)
           o.y (fround v.y)
           o.z (fround v.z)
           o.w (fround v.w)))
  out)

(defspecialization (round :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (round in (the vec4 (vec4))))

(defspecialization (abs :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:abs v.x)
           o.y (cl:abs v.y)
           o.z (cl:abs v.z)
           o.w (cl:abs v.w)))
  out)

(defspecialization (abs :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (abs in (the vec4 (vec4))))

(defspecialization (min :inline t) ((in1 vec4) (in2 vec4) (out vec4)) vec4
  (with-vec4 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:min v1.x v2.x)
           o.y (cl:min v1.y v2.y)
           o.z (cl:min v1.z v2.z)
           o.w (cl:min v1.w v2.w)))
  out)

(defspecialization (min :inline t) ((in1 vec4) (in2 vec4) (out null)) vec4
  (declare (ignore out))
  (min in1 in2 (the vec4 (vec4))))

(defspecialization (max :inline t) ((in1 vec4) (in2 vec4) (out vec4)) vec4
  (with-vec4 ((v1 in1) (v2 in2) (o out))
    (psetf o.x (cl:max v1.x v2.x)
           o.y (cl:max v1.y v2.y)
           o.z (cl:max v1.z v2.z)
           o.w (cl:max v1.w v2.w)))
  out)

(defspecialization (max :inline t) ((in1 vec4) (in2 vec4) (out null)) vec4
  (declare (ignore out))
  (max in1 in2 (the vec4 (vec4))))

(defspecialization (sin :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:sin v.x)
           o.y (cl:sin v.y)
           o.z (cl:sin v.z)
           o.w (cl:sin v.w)))
  out)

(defspecialization (sin :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (sin in (the vec4 (vec4))))

(defspecialization (cos :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:cos v.x)
           o.y (cl:cos v.y)
           o.z (cl:cos v.z)
           o.w (cl:cos v.w)))
  out)

(defspecialization (cos :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (cos in (the vec4 (vec4))))

(defspecialization (tan :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:tan v.x)
           o.y (cl:tan v.y)
           o.z (cl:tan v.z)
           o.w (cl:tan v.w)))
  out)

(defspecialization (tan :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (tan in (the vec4 (vec4))))

(defspecialization (asin :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:asin v.x)
           o.y (cl:asin v.y)
           o.z (cl:asin v.z)
           o.w (cl:asin v.w)))
  out)

(defspecialization (asin :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (asin in (the vec4 (vec4))))

(defspecialization (acos :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:acos v.x)
           o.y (cl:acos v.y)
           o.z (cl:acos v.z)
           o.w (cl:acos v.w)))
  out)

(defspecialization (acos :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (acos in (the vec4 (vec4))))

(defspecialization (atan :inline t) ((in vec4) (out vec4)) vec4
  (with-vec4 ((v in) (o out))
    (psetf o.x (cl:atan v.x)
           o.y (cl:atan v.y)
           o.z (cl:atan v.z)
           o.w (cl:atan v.w)))
  out)

(defspecialization (atan :inline t) ((in vec4) (out null)) vec4
  (declare (ignore out))
  (atan in (the vec4 (vec4))))

(defspecialization (direction= :inline t) ((in1 vec4) (in2 vec4)) boolean
  (cl:>= (dot (normalize in1) (normalize in2))
         (cl:- 1 1e-7)))
