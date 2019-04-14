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
  (:default-initargs :current (m:vec3)
                     :incremental (m:vec3)
                     :incremental-delta (m:vec3)
                     :previous (m:vec3)
                     :interpolated (m:vec3)))

(defclass transform-state-quaternion (transform-state)
  ()
  (:default-initargs :current (m:quat 1)
                     :incremental (m:quat 1)
                     :incremental-delta (m:quat 1)
                     :previous (m:quat 1)
                     :interpolated (m:quat 1)))

(defun make-translation-state ()
  (make-instance 'transform-state-vector))

(defun make-rotation-state ()
  (make-instance 'transform-state-quaternion
                 :incremental (m:vec3)
                 :incremental-delta (m:vec3)))

(defun make-scaling-state ()
  (make-instance 'transform-state-vector :current (m:vec3 1)))

(defun interpolate-vector (state factor)
  (declare (optimize speed))
  (m:lerp (previous state) (current state) factor (interpolated state)))

(defun interpolate-quaternion (state factor)
  (declare (optimize speed))
  (m:slerp (previous state) (current state) factor (interpolated state)))

(define-component transform ()
  ((parent :default nil)
   (children :default nil)
   (translation :default (make-translation-state))
   (rotation :default (make-rotation-state))
   (scaling :default (make-scaling-state))
   (local :default (m:mat4 1))
   (model :default (m:mat4 1))))

(defun transform-add-child (parent child)
  (pushnew child (children parent))
  (setf (parent child) parent))

(defun transform-remove-child (parent child)
  (setf (children parent) (remove-if
                           (lambda (x)
                             (eq x child))
                           (children parent))
        (parent child) nil))

(defun transform-node/vector (state delta)
  (declare (optimize speed))
  (with-slots (%previous %current %incremental-delta %incremental) state
    (m:copy-into %previous %current)
    (m:* %incremental delta %incremental-delta)
    (m:+ %current %incremental-delta %current)))

(defun transform-node/quat (state delta)
  (declare (optimize speed))
  (with-slots (%previous %current %incremental-delta %incremental) state
    (m:copy-into %previous %current)
    (m:* %incremental delta %incremental-delta)
    (m:rotate :local %current %incremental-delta %current)))

(defun transform-node (core node)
  (declare (optimize speed)
           (inline transform-node/vector transform-node/quat))
  (let ((delta (delta (context core))))
    (transform-node/vector (scaling node) delta)
    (transform-node/quat (rotation node) delta)
    (transform-node/vector (translation node) delta)))

(defun resolve-local (node alpha)
  (declare (optimize speed))
  (with-accessors ((local local)
                   (scaling scaling)
                   (rotation rotation)
                   (translation translation))
      node
    (interpolate-vector scaling alpha)
    (interpolate-quaternion rotation alpha)
    (interpolate-vector translation alpha)
    (m:* (m:copy-into local (m:mat4 (interpolated rotation)))
         (m:set-scale m:+id-mat4+ (interpolated scaling))
         local)
    (m:set-translation local (interpolated translation) local)))

(defun resolve-model (node alpha)
  (declare (optimize speed))
  (au:when-let ((parent (parent node)))
    (resolve-local node alpha)
    (m:* (model parent) (local node) (model node))))

(defun map-nodes (func parent)
  (declare (optimize speed)
           (type function func))
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

(defun interpolate-transforms (core)
  (declare (optimize speed))
  (map-nodes
   (lambda (node)
     (resolve-model node (%fl:alpha (%fl:frame-manager core))))
   (actor-component-by-type (%fl:scene-tree core) 'transform)))

(defmethod make-component (context (component-type (eql 'transform)) &rest args)
  (let ((instance (make-instance component-type
                                 :type component-type
                                 :context context)))
    (apply #'reinitialize-instance instance args)
    instance))

(defmethod reinitialize-instance ((instance transform)
                                  &key
                                    actor
                                    (translate (m:vec3))
                                    (translate/inc (m:vec3))
                                    (rotate (m:vec3))
                                    (rotate/inc (m:vec3))
                                    (scale (m:vec3 1))
                                    (scale/inc (m:vec3)))
  (with-accessors ((translation translation)
                   (rotation rotation)
                   (scaling scaling))
      instance
    (setf (actor instance) actor
          (state instance) :initialize
          (current translation) translate
          (previous translation) (m:copy translate)
          (incremental translation) translate/inc
          (current rotation) (etypecase rotate
                               (m:vec3 (m:rotate :local m:+id-quat+ rotate))
                               (m:quat rotate))
          (previous rotation) (m:copy (current rotation))
          (incremental rotation) rotate/inc
          (current scaling) (etypecase scale
                              (m:vec3 scale)
                              (real (m:vec3 scale)))
          (previous scaling) (m:copy (current scaling))
          (incremental scaling) scale/inc)))

;;; User protocol

(defun %rotate/model-space (rotation vec &optional replace-p instant-p)
  (with-accessors ((previous previous) (current current)) rotation
    (m:rotate :local (if replace-p m:+id-quat+ current) vec current)
    (when instant-p
      (m:copy-into previous current))))

(defun %rotate/world-space (rotation vec &optional replace-p instant-p)
  (declare (ignore rotation vec replace-p instant-p))
  (error "ROTATE not yet implemented for world space."))

(defun rotate (transform vec &key (space :model) replace-p instant-p)
  (ecase space
    (:model (%rotate/model-space (rotation transform) vec replace-p instant-p))
    (:world (%rotate/world-space
             (rotation transform) vec replace-p instant-p))))

(defun %translate/model-space (translation vec &optional replace-p instant-p)
  (with-accessors ((previous previous) (current current)) translation
    (m:+ (if replace-p m:+zero-vec3+ current) vec current)
    (when instant-p
      (m:copy-into previous current))))

(defun %translate/world-space (translation vec &optional replace-p instant-p)
  (declare (ignore translation vec replace-p instant-p))
  (error "TRANSLATE not yet implemented for world space."))

(defun translate (transform vec &key (space :model) replace-p instant-p)
  (ecase space
    (:model (%translate/model-space
             (translation transform) vec replace-p instant-p))
    (:world (%translate/world-space
             (translation transform) vec replace-p instant-p))))

(defun scale (transform vec &key replace-p instant-p)
  (with-accessors ((previous previous) (current current)) (scaling transform)
    (m:+ (if replace-p m:+zero-vec3+ current) vec current)
    (when instant-p
      (m:copy-into previous current))))

;; TODO: Try and get rid of any produced garbage in the functions below.

(defun transform-point (transform point-vec)
  "Transform the vector in POINT-VEC, assumed to be in the local space of the
TRANSFORM, to world space and returns the new vector. The new vector is affected
by scale, rotation, and translation. A newly allocated M:VEC3 is returned."
  (m:vec3 (m:* (model transform) (m:vec4 point-vec 1))))

(defun inverse-transform-point (transform point-vec)
  "Transform the vector in POINT-VEC, assumed to be in the world space, to the
local space of the TRANSFORM and returns the new vector. The new vector is
affected by scale, rotation, and translation. A newly allocated M:VEC3 is
returned."
  (m:vec3 (m:* (m:invert (model transform)) (m:vec4 point-vec 1))))

(defun transform-vector (transform vector-vec)
  "Transform the vector in VECTOR-VEC, assumed to be in the local space of the
TRANSFORM, to world space and return it. The new vector is affected by scale and
rotation, but not by translation. A newly allocated M:VEC3 is returned."
  (let ((zero-translation-model (m:copy (model transform))))

    (m:set-translation zero-translation-model (m:vec3)
                       zero-translation-model)

    (m:vec3 (m:* zero-translation-model (m:vec4 vector-vec 1)))))

(defun inverse-transform-vector (transform vector-vec)
  "Transform the vector in VECTOR-VEC, assumed to be in the world space,
to the local space of the TRANSFORM and return it. The new vector is affected by
scale and rotation, but not by translation. A newly allocated M:VEC3 is
returned."
  (let ((zero-translation-model (m:copy (model transform))))

    (m:set-translation zero-translation-model (m:vec3)
                       zero-translation-model)

    (m:vec3 (m:* (m:invert zero-translation-model) (m:vec4 vector-vec 1)))))


(defun transform-direction (transform direction-vec)
  "Transform the vector in DIRECTION-VEC, assumed to be in the local space of
the TRANSFORM, to the world space and return it. The new vector is affected only
by rotation, and not by translation or scale. A newly allocated M:VEC3 is
returned."
  (let ((zero-translation-identity-scale-model (m:copy (model transform))))

    (m:set-translation zero-translation-identity-scale-model (m:vec3)
                       zero-translation-identity-scale-model)

    ;; TODO: Just need to normalize the rotation portion, not orthonormalize it
    (m::orthonormalize zero-translation-identity-scale-model
                       zero-translation-identity-scale-model)

    (m:vec3 (m:* zero-translation-identity-scale-model
                 (m:vec4 direction-vec 1)))))

(defun inverse-transform-direction (transform direction-vec)
  "Transform the vector in DIRECTION-VEC, assumed to be in world space,
to the local space of the TRANSFORM and return it. The new vector is affected
only by rotation, and not by translation or scale. A newly allocated M:VEC3 is
returned."
  (let ((zero-translation-identity-scale-model (m:copy (model transform))))

    (m:set-translation zero-translation-identity-scale-model (m:vec3)
                       zero-translation-identity-scale-model)

    ;; TODO: Just need to normalize the rotation portion, not also
    ;; make it ortho, but there is no math function for it. We need to
    ;; write one.
    (m::orthonormalize zero-translation-identity-scale-model
                       zero-translation-identity-scale-model)

    (m:vec3 (m:* (m:invert zero-translation-identity-scale-model)
                 (m:vec4 direction-vec 1)))))

;; NOTE: These functions return the vectors that represent
;; forward, backward, up, down, right, and left in _world space_.
;;
;; We define these axes as the directions for an object in FL:
;;
;; +z back, -z forward, +y up, -y down, +x right, -x left

(defun transform-forward (transform)
  "Return the forward vector (-Z axis) in world space for this TRANSFORM."
  (m:negate (m:vec3 (m:get-column (model transform) 2))))

(defun transform-backward (transform)
  "Return the backward vector (+Z axis) in world space for this TRANSFORM."
  (m:vec3 (m:get-column (model transform) 2)))

(defun transform-up (transform)
  "Return the up vector (+Y axis) in world space for this TRANSFORM."
  (m:vec3 (m:get-column (model transform) 1)))

(defun transform-down (transform)
  "Return the down vector (-Y axis) in world space for this TRANSFORM."
  (m:negate (m:vec3 (m:get-column (model transform) 1))))

(defun transform-right (transform)
  "Return the right vector (+X axis) in world space for this TRANSFORM."
  (m:vec3 (m:get-column (model transform) 0)))

(defun transform-left (transform)
  "Return the left vector (+X axis) in world space for this TRANSFORM."
  (m:negate (m:vec3 (m:get-column (model transform) 0))))
