(in-package #:first-light.components)

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
  (:default-initargs :current (v3:zero)
                     :incremental (v3:zero)
                     :incremental-delta (v3:zero)
                     :previous (v3:zero)
                     :interpolated (v3:zero)))

(defclass transform-state-quaternion (transform-state)
  ()
  (:default-initargs :current (q:id)
                     :incremental (q:id)
                     :incremental-delta (q:id)
                     :previous (q:id)
                     :interpolated (q:id)))

(defun make-translation-state ()
  (make-instance 'transform-state-vector))

(defun make-rotation-state ()
  (make-instance 'transform-state-quaternion))

(defun make-scaling-state ()
  (make-instance 'transform-state-vector :current (v3:one)))

(defun interpolate-vector (state factor)
  (declare (optimize speed))
  (v3:lerp! (interpolated state) (previous state) (current state) factor))

(defun interpolate-quaternion (state factor)
  (declare (optimize speed))
  (q:slerp! (interpolated state) (previous state) (current state) factor))

(define-component transform ()
  ((parent :default nil)
   (children :default nil)
   (translation :default (make-translation-state))
   (rotation :default (make-rotation-state))
   (scaling :default (make-scaling-state))
   (local :default (m4:id))
   (model :default (m4:id))))

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
    (v3:copy! %previous %current)
    (v3:scale! %incremental-delta %incremental delta)
    (v3:+! %current %current %incremental-delta)))

(defun transform-node/quat (state delta)
  (declare (optimize speed))
  (with-slots (%previous %current %incremental-delta %incremental) state
    (q:copy! %previous %current)
    (q:slerp! %incremental-delta q:+id+ %incremental delta)
    (q:rotate! %current %current %incremental-delta)))

(defun transform-node (core node)
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
    (m4:*! local
           (m4:copy! local (q:to-mat4 (interpolated rotation)))
           (m4:set-scale m4:+id+ (interpolated scaling)))
    (m4:set-translation! local local (interpolated translation))))

(defun resolve-model (node alpha)
  (declare (optimize speed))
  (a:when-let ((parent (parent node)))
    (resolve-local node alpha)
    (m4:*! (model node) (model parent) (local node))))

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
                                    (translate (v3:zero))
                                    (translate/inc (v3:zero))
                                    (rotate (q:id))
                                    (rotate/inc (q:id))
                                    (scale (v3:one))
                                    (scale/inc (v3:zero)))
  (with-accessors ((translation translation)
                   (rotation rotation)
                   (scaling scaling))
      instance
    (setf (actor instance) actor
          (state instance) :initialize

          (current translation) (v3:copy translate)
          (previous translation) (v3:copy (current translation))
          (incremental translation) (v3:copy translate/inc)

          (current rotation) (q:copy rotate)
          (previous rotation) (q:copy (current rotation))
          (incremental rotation) (q:copy rotate/inc)

          (current scaling) (etypecase scale
                              (v3:vec (v3:copy scale))
                              (real (v3:make scale scale scale)))
          (previous scaling) (v3:copy (current scaling))
          (incremental scaling) (v3:copy scale/inc))))

;;; User protocol

(defun %rotate/model-space (rotation rot &optional replace-p instant-p)
  (with-accessors ((previous previous) (current current)) rotation
    (q:rotate! current (if replace-p q:+id+ current) rot)
    (when instant-p
      (q:copy! previous current))))

(defun %rotate/world-space (rotation rot &optional replace-p instant-p)
  (declare (ignore rotation rot replace-p instant-p))
  (error "ROTATE not yet implemented for world space."))

(defun rotate (transform rot &key (space :model) replace-p instant-p)
  (ecase space
    (:model (%rotate/model-space (rotation transform) rot replace-p instant-p))
    (:world (%rotate/world-space
             (rotation transform) rot replace-p instant-p))))

(defun %translate/model-space (translation vec &optional replace-p instant-p)
  (with-accessors ((previous previous) (current current)) translation
    (v3:+! current (if replace-p v3:+zero+ current) vec)
    (when instant-p
      (v3:copy! previous current))))

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
    (v3:+! current (if replace-p v3:+zero+ current) vec)
    (when instant-p
      (v3:copy! previous current))))

;; TODO: Try and get rid of any produced garbage in the functions below.

(defun transform-point (transform point-vec)
  "Transform the vector in POINT-VEC, assumed to be in the local space of the
TRANSFORM, to world space and returns the new vector. The new vector is affected
by scale, rotation, and translation. A newly allocated M:VEC3 is returned."
  (v3:with-components ((v point-vec))
    (~:.xyz (m4:*v4 (model transform) (v4:make vx vy vz 1)))))

(defun inverse-transform-point (transform point-vec)
  "Transform the vector in POINT-VEC, assumed to be in the world space, to the
local space of the TRANSFORM and returns the new vector. The new vector is
affected by scale, rotation, and translation. A newly allocated M:VEC3 is
returned."
  (v3:with-components ((v point-vec))
    (~:.xyz (m4:*v4 (m4:invert (model transform)) (v4:make vx vy vz 1)))))

(defun transform-vector (transform vector-vec)
  "Transform the vector in VECTOR-VEC, assumed to be in the local space of the
TRANSFORM, to world space and return it. The new vector is affected by scale and
rotation, but not by translation. A newly allocated M:VEC3 is returned."
  (v3:with-components ((v vector-vec))
    (let ((zero-translation-model (m4:copy (model transform))))
      (m4:set-translation! zero-translation-model
                           zero-translation-model
                           (v3:zero))
      (~:.xyz (m4:*v4 zero-translation-model (v4:make vx vy vz 1))))))

(defun inverse-transform-vector (transform vector-vec)
  "Transform the vector in VECTOR-VEC, assumed to be in the world space,
to the local space of the TRANSFORM and return it. The new vector is affected by
scale and rotation, but not by translation. A newly allocated M:VEC3 is
returned."
  (v3:with-components ((v vector-vec))
    (let ((zero-translation-model (m4:copy (model transform))))
      (m4:set-translation! zero-translation-model
                           zero-translation-model
                           (v3:zero))
      (~:.xyz (m4:*v4 (m4:invert zero-translation-model)
                      (v4:make vx vy vz 1))))))


(defun transform-direction (transform direction-vec)
  "Transform the vector in DIRECTION-VEC, assumed to be in the local space of
the TRANSFORM, to the world space and return it. The new vector is affected only
by rotation, and not by translation or scale. A newly allocated M:VEC3 is
returned."
  (v3:with-components ((v direction-vec))
    (let ((zero-translation-identity-scale-model (m4:copy (model transform))))
      (m4:set-translation! zero-translation-identity-scale-model
                           zero-translation-identity-scale-model
                           (v3:zero))
      ;; TODO: Just need to normalize the rotation portion, not orthonormalize it
      (m4:orthonormalize! zero-translation-identity-scale-model
                          zero-translation-identity-scale-model)
      (~:.xyz (m4:*v4 zero-translation-identity-scale-model
                      (v4:make vx vy vz 1))))))

(defun inverse-transform-direction (transform direction-vec)
  "Transform the vector in DIRECTION-VEC, assumed to be in world space,
to the local space of the TRANSFORM and return it. The new vector is affected
only by rotation, and not by translation or scale. A newly allocated M:VEC3 is
returned."
  (v3:with-components ((v direction-vec))
    (let ((zero-translation-identity-scale-model (m4:copy (model transform))))
      (m4:set-translation! zero-translation-identity-scale-model
                           zero-translation-identity-scale-model
                           (v3:zero))
      ;; TODO: Just need to normalize the rotation portion, not also
      ;; make it ortho, but there is no math function for it. We need to
      ;; write one.
      (m4:orthonormalize! zero-translation-identity-scale-model
                          zero-translation-identity-scale-model)
      (~:.xyz (m4:*v4 (m4:invert zero-translation-identity-scale-model)
                      (v4:make vx vy vz 1))))))

;; NOTE: These functions return the vectors that represent
;; forward, backward, up, down, right, and left in _world space_.
;;
;; We define these axes as the directions for an object in FL:
;;
;; +z back, -z forward, +y up, -y down, +x right, -x left

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