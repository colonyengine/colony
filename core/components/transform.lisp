(in-package :first-light.components)

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

(defclass transform-state-vector (transform-state)
  ()
  (:default-initargs :current (flm:vec3)
                     :incremental (flm:vec3)
                     :incremental-delta (flm:vec3)
                     :previous (flm:vec3)
                     :interpolated (flm:vec3)))

(defclass transform-state-quaternion (transform-state)
  ()
  (:default-initargs :current (flm:quat 1)
                     :incremental (flm:quat 1)
                     :incremental-delta (flm:quat 1)
                     :previous (flm:quat 1)
                     :interpolated (flm:quat 1)))

(defun make-translation-state ()
  (make-instance 'transform-state-vector))

(defun make-rotation-state ()
  (make-instance 'transform-state-quaternion
                 :incremental (flm:vec3)
                 :incremental-delta (flm:vec3)))

(defun make-scaling-state ()
  (make-instance 'transform-state-vector
                 :current (flm:vec3 1)))

(defun interpolate-vector (state factor)
  (flm:lerp (previous state) (current state) factor (interpolated state)))

(defun interpolate-quaternion (state factor)
  (flm:slerp (previous state) (current state) factor (interpolated state)))

(define-component transform ()
  ((parent :default nil)
   (children :default nil)
   (translation :default (make-translation-state))
   (rotation :default (make-rotation-state))
   (scaling :default (make-scaling-state))
   (local :default (flm:mat4 1))
   (model :default (flm:mat4 1))))

(defun add-child (parent child)
  (push child (children parent))
  (setf (parent child) parent))

(defun remove-child (parent child)
  (setf (children parent) (remove-if (lambda (x) (eq x child)) (children parent))
        (parent child) nil))

(defun transform-node (core-state node)
  (let ((delta (delta (context core-state))))
    (with-slots (%previous %current %incremental-delta %incremental) (scaling node)
      (flm:copy-into %previous %current)
      (flm:+ %current (flm:* %incremental delta %incremental-delta) %current))
    (with-slots (%previous %current %incremental-delta %incremental) (rotation node)
      (flm:copy-into %previous %current)
      (flm:rotate :local %current
                  (flm:* %incremental delta %incremental-delta) %current))


    (with-slots (%previous %current %incremental-delta %incremental) (translation node)
      (flm:copy-into %previous %current)
      (flm:+ %current (flm:* %incremental delta %incremental-delta) %current))))

(defun resolve-local (node alpha)
  (with-slots (%local %scaling %rotation %translation) node
    (interpolate-vector %scaling alpha)
    (interpolate-quaternion %rotation alpha)
    (interpolate-vector %translation alpha)
    (flm:* (flm:mat4 (interpolated %rotation) %local)
           (flm:set-scale flm:+id-mat4+ (interpolated %scaling))
           %local)
    (flm:set-translation %local (interpolated %translation) %local)))

(defun resolve-model (node alpha)
  (fl.util:when-let ((parent (parent node)))
    (resolve-local node alpha)
    (flm:* (model parent) (local node) (model node))))

(defun map-nodes (func parent)
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

(defun interpolate-transforms (core-state)
  (map-nodes
   (lambda (node)
     (resolve-model node (%fl::alpha (%fl:frame-manager core-state))))
   (actor-component-by-type (%fl:scene-tree core-state) 'transform)))

(defmethod make-component (context (component-type (eql 'transform)) &rest args)
  (let ((instance (make-instance component-type :type component-type :context context)))
    (apply #'reinitialize-instance instance args)
    instance))

(defmethod reinitialize-instance ((instance transform)
                                  &key
                                    actor
                                    (translation/current (flm:vec3))
                                    (translation/incremental (flm:vec3))
                                    (rotation/current (flm:vec3))
                                    (rotation/incremental (flm:vec3))
                                    (scale/current (flm:vec3 1))
                                    (scale/incremental (flm:vec3)))
  (with-slots (%translation %rotation %scaling) instance
    (setf (actor instance) actor
          (state instance) :initialize
          (current %translation) translation/current
          (previous %translation) (flm:copy translation/current)
          (incremental %translation) translation/incremental
          (current %rotation) (etypecase rotation/current
                                (flm:vec3
                                 (flm:rotate :local flm:+id-quat+
                                             rotation/current))
                                (flm:quat rotation/current))
          (previous %rotation) (flm:copy (current %rotation))
          (incremental %rotation) rotation/incremental
          (current %scaling) scale/current
          (previous %scaling) (flm:copy scale/current)
          (incremental %scaling) scale/incremental)))

;;; User protocol

(defun %rotate/model-space (rotation vec &optional replace-p instant-p)
  (with-accessors ((previous previous) (current current)) rotation
    (flm:rotate :local (if replace-p flm:+id-quat+ current) vec current)
    (when instant-p
      (flm:copy-into previous current))))

(defun %rotate/world-space (rotation vec &optional replace-p instant-p)
  (declare (ignore rotation vec replace-p instant-p))
  (error "ROTATE not yet implemented for world space."))

(defun rotate (transform vec &key (space :model) replace-p (instant-p t))
  (ecase space
    (:model (%rotate/model-space (rotation transform) vec replace-p instant-p))
    (:world (%rotate/world-space (rotation transform) vec replace-p instant-p))))

(defun %translate/model-space (translation vec &optional replace-p instant-p)
  (with-accessors ((previous previous) (current current)) translation
    (flm:+ (if replace-p flm:+zero-vec3+ current) vec current)
    (when instant-p
      (flm:copy-into previous current))))

(defun %translate/world-space (translation vec &optional replace-p instant-p)
  (declare (ignore translation vec replace-p instant-p))
  (error "TRANSLATE not yet implemented for world space."))

(defun translate (transform vec &key (space :model) replace-p (instant-p t))
  (ecase space
    (:model (%translate/model-space (translation transform) vec replace-p instant-p))
    (:world (%translate/world-space (translation transform) vec replace-p instant-p))))

(defun scale (transform vec &key replace-p (instant-p t))
  (with-accessors ((previous previous) (current current)) (scaling transform)
    (flm:+ (if replace-p flm:+zero-vec3+ current) vec current)
    (when instant-p
      (flm:copy-into previous current))))
