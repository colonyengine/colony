(in-package :fl.comp.transform)

(define-component transform ()
  (parent :default nil)
  (children :default nil)
  (translation :default (%make-transform-state 'transform-state-vector))
  (rotation :default (%make-transform-state 'transform-state-quaternion :incremental (v3:zero)))
  (scale :default (%make-transform-state 'transform-state-vector :current (v3:make 1.0 1.0 1.0)))
  (local :default (m4:id))
  (model :default (m4:id)))

(defun add-child (parent child)
  (push child (children parent))
  (setf (parent child) parent))

(defun remove-child (parent child)
  (setf (children parent) (remove-if (lambda (c) (eq c child)) (children parent))
        (parent child) nil))

(defun translate-node (node delta)
  (with-slots (%current %incremental %previous) (translation node)
    (v3:copy! %previous %current)
    (v3:+! %current %current (v3:scale %incremental delta))))

(defun rotate-node (node delta)
  (with-slots (%current %incremental %previous) (rotation node)
    (q:copy! %previous %current)
    (q:rotate! %current %current (v3:scale %incremental delta))))

(defun scale-node (node delta)
  (with-slots (%current %incremental %previous) (scale node)
    (v3:copy! %previous %current)
    (v3:+! %current %current (v3:scale %incremental delta))))

(defun transform-node (core-state node)
  (let ((delta (box.frame:delta (display core-state))))
    (scale-node node delta)
    (rotate-node node delta)
    (translate-node node delta)))

(defun resolve-local (node alpha)
  (with-slots (%scale %rotation %translation %local) node
    (interpolate-state %scale alpha)
    (interpolate-state %rotation alpha)
    (interpolate-state %translation alpha)
    (m4:*! %local
           (q:to-mat4! %local (interpolated %rotation))
           (m4:scale-from-vec3 m4:+id+ (interpolated %scale)))
    (m4:translation-from-vec3! %local (interpolated %translation))))

(defun resolve-model (node alpha)
  (with-slots (%parent %local %model) node
    (when %parent
      (resolve-local node alpha)
      (m4:*! %model (model %parent) %local)
      %model)))

(defun map-nodes (func parent)
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

(defun interpolate-transforms (root-node alpha)
  (map-nodes
   (lambda (node)
     (resolve-model node alpha))
   root-node))

(defmethod make-component ((component-type (eql 'transform)) context &rest args)
  (let ((instance (make-instance component-type :type component-type)))
    (apply #'reinitialize-instance instance :type component-type args)
    instance))

(defmethod reinitialize-instance ((instance transform)
                                  &key
                                    actor
                                    (translation/current (v3:zero))
                                    (translation/incremental (v3:zero))
                                    (rotation/current (v3:zero))
                                    (rotation/incremental (v3:zero))
                                    (scale/current (v3:make 1.0 1.0 1.0))
                                    (scale/incremental (v3:zero)))
  (with-slots (%translation %rotation %scale) instance
    (setf (actor instance) actor
          (state instance) :initialize
          (current %translation) translation/current
          (incremental %translation) translation/incremental
          (current %rotation) (q:rotate q:+id+ rotation/current)
          (incremental %rotation) rotation/incremental
          (current %scale) scale/current
          (incremental %scale) scale/incremental)))

(defclass transform-state ()
  ((%current :accessor current
             :initarg :current)
   (%incremental :accessor incremental
                 :initarg :incremental)
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
    (v3:zero))
  (:method ((type (eql 'transform-state-quaternion)))
    (q:id)))

(defun %generate-default-state-initargs (type)
  (mapcan
   (lambda (key) (list key (%generate-default-state-value type)))
   '(:current :incremental :previous :interpolated)))

(defun %make-transform-state (type &rest initargs)
  (apply #'make-instance type
         (append initargs (%generate-default-state-initargs type))))

(defgeneric interpolate-state (state factor))

(defmethod interpolate-state ((state transform-state-scalar) factor)
  (with-slots (%previous %current %interpolated) state
    (setf %interpolated (au:lerp factor %previous %current))))

(defmethod interpolate-state ((state transform-state-vector) factor)
  (with-slots (%previous %current %interpolated) state
    (v3:lerp! %interpolated %previous %current factor)))

(defmethod interpolate-state ((state transform-state-quaternion) factor)
  (with-slots (%previous %current %interpolated) state
    (q:slerp! %interpolated %previous %current factor)))
