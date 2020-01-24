(in-package #:virality.components.transform)

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

(defclass transform-state-vector (transform-state) ()
  (:default-initargs :current (v3:vec)
                     :incremental (v3:vec)
                     :incremental-delta (v3:vec)
                     :previous (v3:vec)
                     :interpolated (v3:vec)))

(defclass transform-state-quaternion (transform-state) ()
  (:default-initargs :current (q:quat 1)
                     :incremental (v3:vec) ;; whole angular-velocity vector
                     :incremental-delta (q:quat 1) ;; quaternion of ang-vel * dt
                     :previous (q:quat 1)
                     :interpolated (q:quat 1)))

(defun make-translation-state ()
  (make-instance 'transform-state-vector))

(defun make-rotation-state ()
  (make-instance 'transform-state-quaternion))

(defun make-scaling-state ()
  (make-instance 'transform-state-vector :current (v3:vec 1)))

(defun interpolate-vector (state factor)
  (declare (optimize speed))
  (v3:lerp! (interpolated state) (previous state) (current state) factor))

(defun interpolate-quaternion (state factor)
  (declare (optimize speed))
  (q:slerp! (interpolated state) (previous state) (current state) factor))

(v:define-component transform ()
  ((%parent :accessor parent
            :initform nil)
   (%children :accessor children
              :initform nil)
   (%translation :reader translation
                 :initform (make-translation-state))
   (%rotation :reader rotation
              :initform (make-rotation-state))
   (%scaling :reader scaling
             :initform (make-scaling-state))
   (%local :reader local
           :initform (m4:mat 1))
   (%model :reader model
           :initform (m4:mat 1))))

(defun add-child (parent child)
  (pushnew child (children parent))
  (setf (parent child) parent))

(defun remove-child (parent child)
  (setf (children parent) (remove-if
                           (lambda (x)
                             (eq x child))
                           (children parent))
        (parent child) nil))

(defun transform-node/vector (state delta)
  (declare (optimize speed))
  (with-slots (%previous %current %incremental-delta %incremental) state
    (v3:copy! %previous %current)
    (v3:scale! %incremental-delta %incremental delta)
    (v3:+! %current %current %incremental-delta)))

(defun transform-node/quat (state delta)
  (declare (optimize speed))
  (with-slots (%previous %current %incremental-delta %incremental) state
    (q:copy! %previous %current)
    (o:velocity->rotation! %incremental-delta %incremental delta)
    (q:rotate! %current %current %incremental-delta)))

(defun transform-node (core node)
  (let ((delta (float (v::clock-delta-time (v::clock core)) 1f0)))
    (transform-node/vector (scaling node) delta)
    (transform-node/quat (rotation node) delta)
    (transform-node/vector (translation node) delta)))

(defun resolve-local (node factor)
  (declare (optimize speed))
  (with-slots (%local %scaling %rotation %translation) node
    (interpolate-vector %scaling factor)
    (interpolate-quaternion %rotation factor)
    (interpolate-vector %translation factor)
    (m4:*! %local
           (m4:copy! %local (q:to-mat4 (interpolated %rotation)))
           (m4:set-scale m4:+id+ (interpolated %scaling)))
    (m4:set-translation! %local %local (interpolated %translation))))

(defun resolve-model (node factor)
  (declare (optimize speed))
  (a:when-let ((parent (parent node)))
    (resolve-local node factor)
    (m4:*! (model node) (model parent) (local node))))

(defun map-nodes (func parent)
  (declare (optimize speed)
           (function func))
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

(defun interpolate-transforms (core)
  (declare (optimize speed))
  (let ((factor (float (v::clock-interpolation-factor (v::clock core)) 1f0)))
    (map-nodes
     (lambda (x)
       (resolve-model x factor))
     (v:component-by-type (v::scene-tree core) 'transform))))

(defmethod v:make-component (context (component-type (eql 'transform))
                             &rest args)
  (let ((instance (make-instance component-type
                                 :type component-type
                                 :context context)))
    (apply #'reinitialize-instance instance args)
    instance))

(defmethod reinitialize-instance ((instance transform)
                                  &key
                                    actor
                                    (translate (v3:vec))
                                    (translate/inc (v3:vec))
                                    (rotate (q:quat 1))
                                    (rotate/inc (v3:vec)) ;; angular-velocity
                                    (scale (v3:vec 1))
                                    (scale/inc (v3:vec)))
  (with-slots (%translation %rotation %scaling) instance
    (setf (v:actor instance) actor
          (v::state instance) :initialize
          (current %translation) (v3:copy translate)
          (previous %translation) (v3:copy (current %translation))
          (incremental %translation) (v3:copy translate/inc)
          (current %rotation) (q:copy rotate)
          (previous %rotation) (q:copy (current %rotation))
          (incremental %rotation) (v3:copy rotate/inc)
          (current %scaling) (etypecase scale
                               (v3:vec (v3:copy scale))
                               (real (v3:vec scale scale scale)))
          (previous %scaling) (v3:copy (current %scaling))
          (incremental %scaling) (v3:copy scale/inc))))

;;; User protocol

(defun %rotate/model-space (rotation rot &optional replace-p instant-p)
  (with-slots (%previous %current) rotation
    (q:rotate! %current (if replace-p q:+id+ %current) rot)
    (when instant-p
      (q:copy! %previous %current))))

;; TODO
(defun %rotate/world-space (rotation rot &optional replace-p instant-p)
  (declare (ignore rotation rot replace-p instant-p))
  (error "ROTATE not yet implemented for world space."))

(defun rotate (transform rot &key (space :model) replace-p instant-p)
  (with-slots (%rotation) transform
    (ecase space
      (:model (%rotate/model-space %rotation rot replace-p instant-p))
      (:world (%rotate/world-space %rotation rot replace-p instant-p)))))

(defun %translate/model-space (translation vec &optional replace-p instant-p)
  (with-slots (%previous %current) translation
    (v3:+! %current (if replace-p v3:+zero+ %current) vec)
    (when instant-p
      (v3:copy! %previous %current))))

;; TODO
(defun %translate/world-space (translation vec &optional replace-p instant-p)
  (declare (ignore translation vec replace-p instant-p))
  (error "TRANSLATE not yet implemented for world space."))

(defun translate (transform vec &key (space :model) replace-p instant-p)
  (with-slots (%translation) transform
    (ecase space
      (:model (%translate/model-space %translation vec replace-p instant-p))
      (:world (%translate/world-space %translation vec replace-p instant-p)))))

(defun scale (transform vec &key replace-p instant-p)
  (with-slots (%previous %current) (scaling transform)
    (v3:+! %current (if replace-p v3:+zero+ %current) vec)
    (when instant-p
      (v3:copy! %previous %current))))

;;; TODO: Try and get rid of any produced garbage in the functions below.

(defun transform-point (transform point-vec)
  "Transform the vector in POINT-VEC, assumed to be in the local space of the
TRANSFORM, to world space and returns the new vector. The new vector is affected
by scale, rotation, and translation. A newly allocated M:VEC3 is returned."
  (v3:with-components ((v point-vec))
    (~:.xyz (m4:*v4 (model transform) (v4:vec vx vy vz 1f0)))))

(defun inverse-transform-point (transform point-vec)
  "Transform the vector in POINT-VEC, assumed to be in the world space, to the
local space of the TRANSFORM and returns the new vector. The new vector is
affected by scale, rotation, and translation. A newly allocated M:VEC3 is
returned."
  (v3:with-components ((v point-vec))
    (~:.xyz (m4:*v4 (m4:invert (model transform)) (v4:vec vx vy vz 1f0)))))

(defun transform-vector (transform vector-vec)
  "Transform the vector in VECTOR-VEC, assumed to be in the local space of the
TRANSFORM, to world space and return it. The new vector is affected by scale and
rotation, but not by translation. A newly allocated M:VEC3 is returned."
  (v3:with-components ((v vector-vec))
    (let ((model (m4:copy (model transform))))
      (m4:set-translation! model model v3:+zero+)
      (~:.xyz (m4:*v4 model (v4:vec vx vy vz 1f0))))))

(defun inverse-transform-vector (transform vector-vec)
  "Transform the vector in VECTOR-VEC, assumed to be in the world space,
to the local space of the TRANSFORM and return it. The new vector is affected by
scale and rotation, but not by translation. A newly allocated M:VEC3 is
returned."
  (v3:with-components ((v vector-vec))
    (let ((model (m4:copy (model transform))))
      (m4:set-translation! model model v3:+zero+)
      (~:.xyz (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1f0))))))

(defun transform-direction (transform direction-vec)
  "Transform the vector in DIRECTION-VEC, assumed to be in the local space of
the TRANSFORM, to the world space and return it. The new vector is affected only
by rotation, and not by translation or scale. A newly allocated M:VEC3 is
returned."
  (v3:with-components ((v direction-vec))
    (let ((model (m4:copy (model transform))))
      (m4:set-translation! model model v3:+zero+)
      (m4:normalize-rotation! model model)
      (~:.xyz (m4:*v4 model (v4:vec vx vy vz 1f0))))))

(defun inverse-transform-direction (transform direction-vec)
  "Transform the vector in DIRECTION-VEC, assumed to be in world space,
to the local space of the TRANSFORM and return it. The new vector is affected
only by rotation, and not by translation or scale. A newly allocated M:VEC3 is
returned."
  (v3:with-components ((v direction-vec))
    (let ((model (m4:copy (model transform))))
      (m4:set-translation! model model v3:+zero+)
      (m4:normalize-rotation! model model)
      (~:.xyz (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1f0))))))

;;; NOTE: These functions return the vectors that represent forward, backward,
;;; up, down, right, and left in _world space_.
;;; We define these axes as the directions for an object:
;;; +z back, -z forward, +y up, -y down, +x right, -x left

(defun transform-forward (transform)
  "Return the forward vector (-Z axis) in world space for this TRANSFORM."
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :z)))

(defun transform-backward (transform)
  "Return the backward vector (+Z axis) in world space for this TRANSFORM."
  (m4:rotation-axis-to-vec3 (model transform) :z))

(defun transform-up (transform)
  "Return the up vector (+Y axis) in world space for this TRANSFORM."
  (m4:rotation-axis-to-vec3 (model transform) :y))

(defun transform-down (transform)
  "Return the down vector (-Y axis) in world space for this TRANSFORM."
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :y)))

(defun transform-right (transform)
  "Return the right vector (+X axis) in world space for this TRANSFORM."
  (m4:rotation-axis-to-vec3 (model transform) :x))

(defun transform-left (transform)
  "Return the left vector (+X axis) in world space for this TRANSFORM."
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :x)))
