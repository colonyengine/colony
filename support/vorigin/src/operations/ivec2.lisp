(in-package #:vorigin.ivec2)

(u:fn-> = (vec vec) boolean)
(declaim (inline =))
(defun = (vec1 vec2)
  ;; TODO, Maybe add REL and ABS back as keyword arguments, but need to
  ;; think about what it actually means.
  "Compare vectors VEC1 and VEC2 for integer equality."
  (declare (optimize speed))
  (com:cwcmp 2 (vec1 vec2) (cl:= vec1 vec2)))

(u:fn-> zero! (vec) vec)
(declaim (inline zero!))
(defun zero! (vec)
  "Modify the vector VEC by setting each of its components to zero."
  (declare (optimize speed))
  (com:cwset 2 vec nil 0)
  vec)

(u:fn-> zero () vec)
(declaim (inline zero))
(defun zero ()
  "Construct a fresh vector with each component set to zero."
  (declare (optimize speed))
  (vec 0 0))

(u:fn-> zero-p (vec) boolean)
(declaim (inline zero-p))
(defun zero-p (vec)
  "Check whether or not the input vector is a zero vector."
  (declare (optimize speed))
  (= vec +zero+))

(u:fn-> ones! (vec) vec)
(declaim (inline ones!))
(defun ones! (vec)
  "Modify the vector VEC by setting each of its components to one."
  (declare (optimize speed))
  (com:cwset 2 vec nil 1)
  vec)

(u:fn-> ones () vec)
(declaim (inline ones))
(defun ones ()
  "Construct a fresh vector with each component set to one."
  (declare (optimize speed))
  (vec 1 1))

(u:fn-> uniform! (vec u:b32) vec)
(declaim (inline uniform!))
(defun uniform! (vec value)
  "Modify the vector VEC by setting each of its components to VALUE."
  (declare (optimize speed))
  (com:cwset 2 vec nil value)
  vec)

(u:fn-> uniform (u:b32) vec)
(declaim (inline uniform))
(defun uniform (value)
  "Construct a fresh vector with each component set to VALUE."
  (declare (optimize speed))
  (vec value value))

(u:fn-> random! (vec u:b32 u:b32) vec)
(declaim (inline random!))
(defun random! (vec min max)
  "Modify vector VEC to have a random integer value for each of its
components, The range of each component is bounded by MIN and MAX."
  (declare (optimize speed))
  (let ((diff (com:smod32- max min)))
    (com:cwset 2 vec nil (com:smod32+ min (cl:random diff))))
  vec)

(u:fn-> random (u:b32 u:b32) vec)
(declaim (inline random))
(defun random (min max)
  "Construct a fresh vector with random integer elements. The range of each
component is bounded by MIN and MAX."
  (declare (optimize speed))
  (random! (zero) min max))

(u:fn-> copy! (vec vec) vec)
(declaim (inline copy!))
(defun copy! (out vec)
  "Modify vector OUT by copying the components of vector VEC into it."
  (declare (optimize speed))
  (com:cwset 2 out vec vec)
  out)

(u:fn-> copy (vec) vec)
(declaim (inline copy))
(defun copy (vec)
  "Construct a fresh vector that is a copy of vector VEC."
  (declare (optimize speed))
  (copy! (zero) vec))

(u:fn-> sign! (vec vec) vec)
(declaim (inline sign!))
(defun sign! (out vec)
  "Modify vector OUT to have its components represent the sign of each
component of vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (signum vec))
  out)

(u:fn-> sign (vec) vec)
(declaim (inline sign))
(defun sign (vec)
  "Construct a fresh vector that has its components represent the sign of
each component of vector VEC."
  (declare (optimize speed))
  (sign! (zero) vec))

(u:fn-> clamp! (vec vec vec vec) vec)
(declaim (inline clamp!))
(defun clamp! (out vec min max)
  "Modify vector OUT to have its components represent the components of
vector VEC, bounded by the components of vectors MIN and MAX."
  (declare (optimize speed))
  (com:cwset 2 out (vec min max) (u:clamp vec min max))
  out)

(u:fn-> clamp (vec vec vec) vec)
(declaim (inline clamp))
(defun clamp (vec min max)
  "Construct a fresh vector that has the components of vector VEC bounded
by the components of vectors MIN and MAX."
  (declare (optimize speed))
  (clamp! (zero) vec min max))

(u:fn-> clamp-range! (vec vec u:b32 u:b32) vec)
(declaim (inline clamp-range!))
(defun clamp-range! (out vec min max)
  "Modify vector OUT to have its components represent the components of
vector VEC, bounded by MIN and MAX."
  (declare (optimize speed))
  (com:cwset 2 out vec (u:clamp vec min max))
  out)

(u:fn-> clamp-range (vec u:b32 u:b32) vec)
(declaim (inline clamp-range))
(defun clamp-range (vec min max)
  "Construct a fresh vector that has the components of vector VEC bounded
by MIN and MAX."
  (declare (optimize speed))
  (clamp-range! (zero) vec min max))


(u:fn-> +! (vec vec vec) vec)
(declaim (inline +!))
(defun +! (out vec1 vec2)
  "Modify vector OUT by performing component-wise addition of vectors VEC1
and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (com:smod32+ vec1 vec2))
  out)

(u:fn-> + (vec vec) vec)
(declaim (inline +))
(defun + (vec1 vec2)
  "Construct a fresh vector by performing component-wise addition of
vectors VEC1 and VEC2."
  (declare (optimize speed))
  (+! (zero) vec1 vec2))

(u:fn-> -! (vec vec vec) vec)
(declaim (inline -!))
(defun -! (out vec1 vec2)
  "Modify vector OUT by performing component-wise subtraction of vectors
VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (com:smod32- vec1 vec2))
  out)

(u:fn-> - (vec vec) vec)
(declaim (inline -))
(defun - (vec1 vec2)
  "Construct a fresh vector by performing component-wise substraction of
vectors VEC1 and VEC2."
  (declare (optimize speed))
  (-! (zero) vec1 vec2))

(u:fn-> *! (vec vec vec) vec)
(declaim (inline *!))
(defun *! (out vec1 vec2)
  "Modify vector OUT by performing component-wise multiplication of vectors
VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (com:smod32* vec1 vec2))
  out)

(u:fn-> * (vec vec) vec)
(declaim (inline *))
(defun * (vec1 vec2)
  "Construct a fresh vector by performing component-wise multiplication of
vectors VEC1 and VEC2."
  (declare (optimize speed))
  (*! (zero) vec1 vec2))

(u:fn-> /! (vec vec vec) vec)
(declaim (inline /!))
(defun /! (out vec1 vec2)
  "Modify vector OUT by performing component-wise truncated towards zero
division of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (if (zerop vec2) 0 (com:smod32/ vec1 vec2)))
  out)

(u:fn-> / (vec vec) vec)
(declaim (inline /))
(defun / (vec1 vec2)
  "Construct a fresh vector by performing component-wise truncated towards
zero division of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (/! (zero) vec1 vec2))

(u:fn-> scale! (vec vec u:b32) vec)
(declaim (inline scale!))
(defun scale! (out vec scalar)
  "Modify vector OUT by multiplying the scalar SCALAR to each component of
vector VEC and then truncating each components towards zero."
  (declare (optimize speed))
  (com:cwset 2 out vec (com:smod32* vec scalar))
  out)

(u:fn-> scale (vec u:b32) vec)
(declaim (inline scale))
(defun scale (vec scalar)
  "Construct a fresh vector by multiplying the scalar SCALAR to each
component of vector VEC and then truncating each component towards zero."
  (declare (optimize speed))
  (scale! (zero) vec scalar))

(u:fn-> dot (vec vec) u:b32)
(declaim (inline dot))
(defun dot (vec1 vec2)
  "Compute the dot product of vectors VEC1 and VEC2. Returns a scalar."
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (com:smod32+ (com:smod32* v1x v2x)
                 (com:smod32* v1y v2y))))

(u:fn-> length-squared (vec) u:b32)
(declaim (inline length-squared))
(defun length-squared (vec)
  "Compute the squared length of vector VEC."
  (declare (optimize speed))
  (with-components ((v vec))
    (com:smod32+ (cl:expt vx 2) (cl:expt vy 2))))

(u:fn-> length (vec) u:f32)
(declaim (inline length))
(defun length (vec)
  "Compute the length of vector VEC. Returns a scalar."
  (declare (optimize speed))
  (cl:sqrt (length-squared vec)))

(u:fn-> round! (vec vec) vec)
(declaim (inline round!))
(defun round! (out vec)
  "Modify vector OUT to have its components be the result of rounding the
components of vector VEC to the nearest whole number. This is an identity
operation."
  (declare (optimize speed))
  (com:cwset 2 out vec vec)
  out)

(u:fn-> round (vec) vec)
(declaim (inline round))
(defun round (vec)
  "Construct a fresh vector that is the result of rounding the components
of vector VEC to the nearest whole number. This is an identity
operation."
  (declare (optimize speed))
  (round! (zero) vec))

(u:fn-> abs! (vec vec) vec)
(declaim (inline abs!))
(defun abs! (out vec)
  "Modify vector OUT to have the absolute value of each component of vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:abs vec))
  out)

(u:fn-> abs (vec) vec)
(declaim (inline abs))
(defun abs (vec)
  "Construct a fresh vector to have the absolute value of each component of
vector VEC."
  (declare (optimize speed))
  (abs! (zero) vec))

(u:fn-> negate! (vec vec) vec)
(declaim (inline negate!))
(defun negate! (out vec)
  "Modify vector OUT to have the components of vector VEC with their signs
negated."
  (declare (optimize speed))
  (scale! out vec -1))

(u:fn-> negate (vec) vec)
(declaim (inline negate))
(defun negate (vec)
  "Construct a fresh vector with the components of vector VEC with their
signs negated."
  (declare (optimize speed))
  (negate! (zero) vec))

(u:fn-> angle (vec vec) u:f32)
(declaim (inline angle))
(defun angle (vec1 vec2)
  "Compute the angle in radians between the vectors VEC1 and VEC2."
  (declare (optimize speed))
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (length vec1) (length vec2))))
    (if (zerop m*m)
        0f0
        (cl:acos (the (u:f32 -1f0 1f0) (cl:/ dot m*m))))))

(u:fn-> lerp! (vec vec vec u:f32) vec)
(declaim (inline lerp!))
(defun lerp! (out vec1 vec2 factor)
  "Modify vector OUT with the result of linearly interpolating between
vectors VEC1 and VEC2 by FACTOR and truncating towards zero the result."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (cl:truncate (u:lerp factor vec1 vec2)))
  out)

(u:fn-> lerp (vec vec u:f32) vec)
(declaim (inline lerp))
(defun lerp (vec1 vec2 factor)
  "Construct a fresh vector that is the result of linearly interpolating
between vectors VEC1 and VEC2 by FACTOR and truncating towards zero the
result."
  (declare (optimize speed))
  (lerp! (zero) vec1 vec2 factor))

(u:fn-> < (vec vec &optional (member :and :or)) boolean)
(declaim (inline <))
(defun < (vec1 vec2 &optional (op :and))
  "Check whether or not each component of vector VEC1 is less than the
respective components of vector VEC2."
  (declare (optimize speed))
  (if (eq op :and)
      (com:cwcmp 2 (vec1 vec2) (cl:< vec1 vec2))
      (com:cwcmp-or 2 (vec1 vec2) (cl:< vec1 vec2))))

(u:fn-> <= (vec vec &optional (member :and :or)) boolean)
(declaim (inline <=))
(defun <= (vec1 vec2 &optional (op :and))
  "Check whether or not each component of vector VEC1 is less than or equal
to the respective components of vector VEC2."
  (declare (optimize speed))
  (if (eq op :and)
      (com:cwcmp 2 (vec1 vec2) (cl:<= vec1 vec2))
      (com:cwcmp-or 2 (vec1 vec2) (cl:<= vec1 vec2))))

(u:fn-> > (vec vec &optional (member :and :or)) boolean)
(declaim (inline >))
(defun > (vec1 vec2 &optional (op :and))
  "Check whether or not each component of vector VEC1 is greater than the
respective components of vector VEC2."
  (declare (optimize speed))
  (if (eq op :and)
      (com:cwcmp 2 (vec1 vec2) (cl:> vec1 vec2))
      (com:cwcmp-or 2 (vec1 vec2) (cl:> vec1 vec2))))

(u:fn-> >= (vec vec &optional (member :and :or)) boolean)
(declaim (inline >=))
(defun >= (vec1 vec2 &optional (op :and))
  "Check whether or not each component of vector VEC1 is greater than or
equal to the respective components of vector VEC2."
  (declare (optimize speed))
  (if (eq op :and)
      (com:cwcmp 2 (vec1 vec2) (cl:>= vec1 vec2))
      (com:cwcmp-or 2 (vec1 vec2) (cl:>= vec1 vec2))))

(u:fn-> min! (vec vec vec) vec)
(declaim (inline min!))
(defun min! (out vec1 vec2)
  "Modify vector OUT to have the minimum value for each component in
vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (cl:min vec1 vec2))
  out)

(u:fn-> min (vec vec) vec)
(declaim (inline min))
(defun min (vec1 vec2)
  "Construct a fresh vector which has the minimum value of each component
in vectors VEC1 and VEC2."
  (declare (optimize speed))
  (min! (zero) vec1 vec2))

(u:fn-> max! (vec vec vec) vec)
(declaim (inline max!))
(defun max! (out vec1 vec2)
  "Modify vector OUT to have the maximum value for each component in
vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (cl:max vec1 vec2))
  out)

(u:fn-> max (vec vec) vec)
(declaim (inline max))
(defun max (vec1 vec2)
  "Construct a fresh vector which has the maximum value of each component
in vectors VEC1 and VEC2."
  (declare (optimize speed))
  (max! (zero) vec1 vec2))

(u:fn-> floor! (vec vec &optional u:b32) vec)
(declaim (inline floor!))
(defun floor! (out vec &optional (divisor 1))
  "Modify vector OUT to have the nearest integer less than or equal to each
component of vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:floor vec divisor))
  out)

(u:fn-> floor (vec &optional u:b32) vec)
(declaim (inline floor))
(defun floor (vec &optional (divisor 1))
  "Construct a fresh vector that has the nearest integer less than or equal
to each component of vector VEC."
  (declare (optimize speed))
  (floor! (zero) vec divisor))

(u:fn-> ceiling! (vec vec &optional u:b32) vec)
(declaim (inline ceiling!))
(defun ceiling! (out vec &optional (divisor 1))
  "Modify vector OUT to have the nearest integer greater than or equal to
each component of vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:ceiling vec divisor))
  out)

(u:fn-> ceiling (vec &optional u:b32) vec)
(declaim (inline ceiling))
(defun ceiling (vec &optional (divisor 1))
  "Construct a fresh vector that has the nearest integer greater than or
equal to each component of vector VEC."
  (declare (optimize speed))
  (ceiling! (zero) vec divisor))

(u:fn-> mod! (vec vec u:b32) vec)
(declaim (inline mod!))
(defun mod! (out vec divisor)
  "Modify vector OUT to have each component of vector VEC modulo DIVISOR."
  (declare (optimize speed))
  (com:cwset 2 out vec (com:smod32-mod vec divisor))
  out)

(u:fn-> mod (vec u:b32) vec)
(declaim (inline mod))
(defun mod (vec divisor)
  "Construct a fresh vector that has each component of vector VEC modulo
DIVISOR."
  (declare (optimize speed))
  (mod! (zero) vec divisor))
