(in-package :gear)

(defclass transform-state ()
  ((%current :accessor current
             :initarg :current)
   (%incremental :accessor incremental
                 :initarg :incremental)
   (%previous :accessor previous
              :initarg :previous)
   (%interpolated :accessor interpolated
                  :initarg :interpolated)
   (%modifiedp :accessor modifiedp
               :initarg :modifiedp)))

(defclass transform-state-scalar (transform-state) ())

(defclass transform-state-vector (transform-state) ())

(defclass transform-state-quaternion (transform-state) ())

(defmethod initialize-instance :after ((object transform-state-quaternion) &key)
  (with-slots (%current) object
    (when (typep %current 'vec)
      (setf %current (qrot (qid) %current)))))

(defgeneric %generate-default-state-value (type)
  (:method ((type (eql 'transform-state-scalar)))
    0)
  (:method ((type (eql 'transform-state-vector)))
    (vec))
  (:method ((type (eql 'transform-state-quaternion)))
    (qid)))

(defun %generate-default-state-initargs (type)
  (mapcan
   (lambda (key) (list key (%generate-default-state-value type)))
   '(:current :incremental :previous :interpolated :modifiedp)))

(defun %make-transform-state (type &rest initargs)
  (apply #'make-instance type
         (append initargs (%generate-default-state-initargs type))))

(defgeneric interpolate-state (state factor))

(defmethod interpolate-state ((state transform-state-scalar) factor)
  (with-slots (%previous %current %interpolated %modifiedp) state
    (if %modifiedp
        (setf %interpolated (lerp factor %previous %current))
        (setf %interpolated %current))))

(defmethod interpolate-state ((state transform-state-vector) factor)
  (with-slots (%previous %current %interpolated %modifiedp) state
    (if %modifiedp
        (vlerp! %interpolated %previous %current factor)
        (vcp! %interpolated %current))))

(defmethod interpolate-state ((state transform-state-quaternion) factor)
  (with-slots (%previous %current %interpolated %modifiedp) state
    (if %modifiedp
        (qslerp! %interpolated %previous %current factor)
        (qcp! %interpolated %current))))
