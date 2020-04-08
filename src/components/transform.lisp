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
   (%scale :reader scale
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
    (v::transform-node/vector (scale node) v:=delta= frame-time)
    (v::transform-node/quaternion (rotation node) v:=delta= frame-time)
    (v::transform-node/vector (translation node) v:=delta= frame-time)))

(defun resolve-local (node factor)
  (with-slots (%local) node
    (let ((translation (translation node))
          (rotation (rotation node))
          (scale (scale node)))
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
  (with-slots (%translation %rotation %scale) instance
    (setf (v:actor instance) actor
          (v::state instance) :initialize)
    (v::initialize-translation %translation translate translate/velocity)
    (v::initialize-rotation %rotation rotate rotate/velocity)
    (v::initialize-scale %scale scale scale/velocity)))

;;; User protocol helper functions
;;; The user protocol functions are generic and live in the VIRALITY.ENGINE
;;; package, but they call out to the helper functions below.

(defun %get-model-matrix (transform copy)
  (let ((model (model transform)))
    (if copy (m4:copy model) model)))

(defun %get-translation (transform copy)
  (let ((translation (v::current (translation transform))))
    (if copy (v3:copy translation) translation)))

(defun %get-rotation (transform copy)
  (let ((rotation (v::current (rotation transform))))
    (if copy (q:copy rotation) rotation)))

(defun %get-scale (transform copy)
  (let ((scale (v::current (rotation transform))))
    (if copy (v3:copy scale) scale)))

(defun %translate (transform vec space replace instant)
  (let ((state (translation transform)))
    (ecase space
      (:model
       (v3:+! (v::current state)
              (if replace v3:+zero+ (v::current state))
              vec)
       (when instant
         (v3:copy! (v::previous state) (v::current state))))
      (:world (error "TRANSLATE not yet implemented for world space.")))))

(defun %translate/velocity (transform axis rate)
  (let ((state (translation transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

(defun %rotate (transform quat space replace instant)
  (let ((state (rotation transform)))
    (ecase space
      (:model
       (q:rotate! (v::current state)
                  (if replace q:+id+ (v::current state))
                  quat)
       (when instant
         (q:copy! (v::previous state) (v::current state))))
      (:world (error "ROTATE not yet implemented for world space.")))))

(defun %rotate/velocity (transform axis rate)
  (let ((state (rotation transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

(defun %scale (transform vec space replace instant)
  (let ((state (scale transform)))
    (ecase space
      (:model
       (v3:+! (v::current state)
              (if replace v3:+zero+ (v::current state))
              vec)
       (when instant
         (v3:copy! (v::previous state) (v::current state))))
      (:world (error "SCALE not yet implemented for world space.")))))

(defun %scale/velocity (transform axis rate)
  (let ((state (scale transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

(defun %transform-point (transform point space)
  (let ((model (model transform)))
    (v3:with-components ((v point))
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun %transform-vector (transform vector space)
  (let ((model (m4:copy (model transform))))
    (v3:with-components ((v vector))
      (m4:set-translation! model model v3:+zero+)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun %transform-direction (transform direction space)
  (let ((model (m4:copy (model transform))))
    (v3:with-components ((v direction))
      (m4:set-translation! model model v3:+zero+)
      (m4:normalize-rotation! model model)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun %transform-forward (transform)
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :z)))

(defun %transform-backward (transform)
  (m4:rotation-axis-to-vec3 (model transform) :z))

(defun %transform-up (transform)
  (m4:rotation-axis-to-vec3 (model transform) :y))

(defun %transform-down (transform)
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :y)))

(defun %transform-right (transform)
  (m4:rotation-axis-to-vec3 (model transform) :x))

(defun %transform-left (transform)
  (v3:negate (m4:rotation-axis-to-vec3 (model transform) :x)))
