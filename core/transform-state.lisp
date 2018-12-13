(in-package :%first-light)

(defclass transform-state ()
  ((%current :accessor current
             :initarg :current)
   (%incremental :accessor incremental
                 :initarg :incremental)
   (%incremental-delta :accessor incremental-delta
                       :initarg :incremental-delta)
   (%previous :accessor previous
              :initarg :previous)
   (%interpolated :accessor interpolated
                  :initarg :interpolated)))

(defclass transform-state-scalar (transform-state) ())

(defclass transform-state-vector (transform-state) ())

(defclass transform-state-quaternion (transform-state) ())

(defgeneric %generate-default-state-value (type)
  (:method ((type (eql 'transform-state-scalar)))
    0)
  (:method ((type (eql 'transform-state-vector)))
    (flm:vec3))
  (:method ((type (eql 'transform-state-quaternion)))
    (flm:quat 1)))

(defun %generate-default-state-initargs (type)
  (mapcan
   (lambda (key) (list key (%generate-default-state-value type)))
   '(:current :incremental :incremental-delta :previous :interpolated)))

(defun make-transform-state (type &rest initargs)
  (apply #'make-instance type
         (append initargs (%generate-default-state-initargs type))))

(defgeneric interpolate-state (state factor))

(defmethod interpolate-state ((state transform-state-scalar) factor)
  (with-slots (%previous %current %interpolated) state
    (setf %interpolated (fl.util:lerp factor %previous %current))))

(defmethod interpolate-state ((state transform-state-vector) factor)
  (with-slots (%previous %current %interpolated) state
    (flm:lerp %previous %current factor %interpolated)))

(defmethod interpolate-state ((state transform-state-quaternion) factor)
  (with-slots (%previous %current %interpolated) state
    (flm:slerp %previous %current factor %interpolated)))
