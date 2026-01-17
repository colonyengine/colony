(in-package #:cl-user)

(defpackage #:vorigin.ivec2
  (:local-nicknames
   (#:com #:vorigin.common)
   (#:const #:vorigin.constants)
   (#:u #:vutils))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:/
   #:random
   #:length
   #:round
   #:abs
   #:<
   #:<=
   #:>
   #:>=
   #:min
   #:max
   #:floor
   #:ceiling
   #:mod)
  (:export
   #:vec
   #:vec*
   #:x
   #:y
   #:with-components
   #:+zero+
   #:+ones+
   #:+up+
   #:+down+
   #:+left+
   #:+right+
   #:zero
   #:zero!
   #:zero-p
   #:ones!
   #:ones
   #:uniform!
   #:uniform
   #:random!
   #:random
   #:copy!
   #:copy
   #:sign!
   #:sign
   #:clamp!
   #:clamp
   #:clamp-range!
   #:clamp-range
   #:=
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:/!
   #:/
   #:scale!
   #:scale
   #:dot
   #:length-squared
   #:length
   #:round!
   #:round
   #:abs!
   #:abs
   #:negate!
   #:negate
   #:angle
   #:lerp!
   #:lerp
   #:<
   #:<=
   #:>
   #:>=
   #:min!
   #:min
   #:max!
   #:max
   #:floor!
   #:floor
   #:ceiling!
   #:ceiling
   #:mod!
   #:mod))

(in-package #:vorigin.ivec2)

(deftype vec () '(u:b32a 2))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (u:once-only (vec)
    `(symbol-macrolet
         ((,prefix ,vec)
          (,(com:make-accessor-symbol prefix "X") (aref ,vec 0))
          (,(com:make-accessor-symbol prefix "Y") (aref ,vec 1)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

;;; Constructor

(u:fn-> vec (u:b32 u:b32) vec)
(declaim (inline vec))
(u:eval-always
  (defun vec (x y)
    (declare (optimize speed))
    (let ((vec (u:make-b32-array 2)))
      (setf (aref vec 0) x
            (aref vec 1) y)
      vec)))

(u:fn-> vec* (real real) vec)
(declaim (inline vec*))
(u:eval-always
  (defun vec* (x y)
    (vec (truncate x) (truncate y))))

;;; Accessors

(u:fn-> x (vec) u:b32)
(declaim (inline x))
(defun x (vec)
  "Read the 'X' component of vector VEC."
  (declare (optimize speed))
  (aref vec 0))

(u:fn-> (setf x) (u:b32 vec) u:b32)
(declaim (inline (setf x)))
(defun (setf x) (value vec)
  "Write VALUE to the 'X' component of vector VEC."
  (declare (optimize speed))
  (setf (aref vec 0) value))

(u:fn-> y (vec) u:b32)
(declaim (inline y))
(defun y (vec)
  "Read the 'Y' component of vector VEC."
  (declare (optimize speed))
  (aref vec 1))

(u:fn-> (setf y) (u:b32 vec) u:b32)
(declaim (inline (setf y)))
(defun (setf y) (value vec)
  "Write VALUE to the 'Y' component of vector VEC."
  (declare (optimize speed))
  (setf (aref vec 1) value))

;;; Constants

(u:define-constant +zero+ (vec 0 0)
  :test #'equalp
  :documentation "Constant representing a 2D zero vector.")

(u:define-constant +ones+ (vec 1 1)
  :test #'equalp
  :documentation "Constant representing a 2D vector with each component being 1.")

(u:define-constant +up+ (vec 0 1)
  :test #'equalp
  :documentation "Constant representing a 2D unit vector facing up.")

(u:define-constant +down+ (vec 0 -1)
  :test #'equalp
  :documentation "Constant representing a 2D unit vector facing down.")

(u:define-constant +left+ (vec -1 0)
  :test #'equalp
  :documentation "Constant representing a 2D unit vector facing left.")

(u:define-constant +right+ (vec 1 0)
  :test #'equalp
  :documentation "Constant representing a 2D unit vector facing right.")
