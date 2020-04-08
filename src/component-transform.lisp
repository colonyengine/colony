(in-package #:virality.components.transform)

(v:define-component transform ()
  ((%parent :accessor parent
            :initform nil)
   (%children :accessor children
              :initform nil)
   (%translation :reader translation
                 :initform (v::make-translate-state))
   (%rotation :reader rotation
              :initform (v::make-rotate-state))
   (%scaling :reader scaling
             :initform (v::make-scale-state))
   (%local :reader local
           :initform (m4:mat 1))
   (%model :reader model
           :initform (m4:mat 1))
   (%normal-matrix :reader normal-matrix
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

(defun transform-node (core node)
  (let ((frame-time (v:frame-time (v:context core))))
    (v::transform-node/vector (scaling node) v:=delta= frame-time)
    (v::transform-node/quaternion (rotation node) v:=delta= frame-time)
    (v::transform-node/vector (translation node) v:=delta= frame-time)))

(defun resolve-local (node factor)
  (with-slots (%local) node
    (let ((translation (translation node))
          (rotation (rotation node))
          (scale (scaling node)))
      (v::interpolate-vector scale factor)
      (v::interpolate-quaternion rotation factor)
      (v::interpolate-vector translation factor)
      (m4:copy! %local (q:to-mat4 (v::interpolated rotation)))
      (m4:*! %local %local (m4:set-scale m4:+id+ (v::interpolated scale)))
      (m4:set-translation! %local %local (v::interpolated translation)))))

(defun resolve-model (node factor)
  (a:when-let ((parent (parent node)))
    (resolve-local node factor)
    (m4:*! (model node) (model parent) (local node))))

(defun map-nodes (func parent)
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

(defun interpolate-transforms (core)
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

(defmethod shared-initialize :after ((instance transform) slot-names
                                     &key
                                       actor
                                       translate
                                       translate/velocity
                                       rotate
                                       rotate/velocity
                                       scale
                                       scale/velocity)
  (with-slots (%translation %rotation %scaling) instance
    (setf (v:actor instance) actor
          (v::state instance) :initialize)
    (v::initialize-translation %translation translate translate/velocity)
    (v::initialize-rotation %rotation rotate rotate/velocity)
    (v::initialize-scale %scaling scale scale/velocity)))

;;; User protocol

(defun get-translation (transform &key copy)
  (let ((translation (v::current (translation transform))))
    (if copy (v3:copy translation) translation)))

(defun get-rotation (transform &key copy)
  (let ((rotation (v::current (rotation transform))))
    (if copy (q:copy rotation) rotation)))

(defun get-scale (transform &key copy)
  (let ((scale (v::current (rotation transform))))
    (if copy (v3:copy scale) scale)))

(defun translate (transform vec &key (space :model) replace-p instant-p)
  (let ((state (translation transform)))
    (ecase space
      (:model
       (v3:+! (v::current state)
              (if replace-p v3:+zero+ (v::current state))
              vec)
       (when instant-p
         (v3:copy! (v::previous state) (v::current state))))
      (:world (error "TRANSLATE not yet implemented for world space.")))))

(defun translate/velocity (transform axis rate)
  (let ((state (translation transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

(defun rotate (transform quat &key (space :model) replace-p instant-p)
  (let ((state (rotation transform)))
    (ecase space
      (:model
       (q:rotate! (v::current state)
                  (if replace-p q:+id+ (v::current state))
                  quat)
       (when instant-p
         (q:copy! (v::previous state) (v::current state))))
      (:world (error "ROTATE not yet implemented for world space.")))))

(defun rotate/velocity (transform axis rate)
  (let ((state (rotation transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

(defun scale (transform vec &key (space :model) replace-p instant-p)
  (let ((state (scaling transform)))
    (ecase space
      (:model
       (v3:+! (v::current state)
              (if replace-p v3:+zero+ (v::current state))
              vec)
       (when instant-p
         (v3:copy! (v::previous state) (v::current state))))
      (:world (error "SCALE not yet implemented for world space.")))))

(defun scale/velocity (transform axis rate)
  (let ((state (scaling transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

;;; TODO: Try and get rid of any produced garbage in the functions below.

(defun transform-point (transform point &key (space :model))
  "Transform the vector in POINT, assumed to be in the local space of the
TRANSFORM, to world space and returns the new vector. The new vector is affected
by scale, rotation, and translation. A newly allocated M:VEC3 is returned."
  (let ((model (model transform)))
    (v3:with-components ((v point))
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun transform-vector (transform vector &key (space :model))
  "Transform the vector in VECTOR, assumed to be in the local space of the
TRANSFORM, to world space and return it. The new vector is affected by scale and
rotation, but not by translation. A newly allocated M:VEC3 is returned."
  (let ((model (m4:copy (model transform))))
    (v3:with-components ((v vector))
      (m4:set-translation! model model v3:+zero+)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun transform-direction (transform direction &key (space :model))
  "Transform the vector in DIRECTION, assumed to be in the local space of the
TRANSFORM, to the world space and return it. The new vector is affected only by
rotation, and not by translation or scale. A newly allocated M:VEC3 is
returned."
  (let ((model (m4:copy (model transform))))
    (v3:with-components ((v direction))
      (m4:set-translation! model model v3:+zero+)
      (m4:normalize-rotation! model model)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

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
