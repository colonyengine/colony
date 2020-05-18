(in-package #:virality.component)

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

(defun transform-node (node)
  (v::transform-node/vector (scale node) v:=delta=)
  (v::transform-node/quaternion (rotation node) v:=delta=)
  (v::transform-node/vector (translation node) v:=delta=))

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

;;; User protocol helper functions The user protocol functions are generic and
;;; live in the VIRALITY package, but they call out to the helper functions
;;; below.

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
  (let ((scale (v::current (scale transform))))
    (if copy (v3:copy scale) scale)))

(defun %translate (transform vec space replace instant)
  (let ((state (translation transform)))
    (ecase space
      (:local
       (v3:+! (v::current state)
              (if replace v3:+zero+ (v::current state))
              vec)
       (when instant
         (v3:copy! (v::previous state) (v::current state))))
      (:model (error "TRANSLATE not yet implemented for world space.")))))

(defun %translate/velocity (transform axis rate)
  (let ((state (translation transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

(defun %rotate (transform quat space replace instant)
  (let ((state (rotation transform)))
    (ecase space
      (:local
       (q:rotate! (v::current state)
                  (if replace q:+id+ (v::current state))
                  quat)
       (when instant
         (q:copy! (v::previous state) (v::current state))))
      (:model (error "ROTATE not yet implemented for world space.")))))

(defun %rotate/velocity (transform axis rate)
  (let ((state (rotation transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

(defun %scale (transform vec space replace instant)
  (let ((state (scale transform)))
    (ecase space
      (:local
       (v3:+! (v::current state)
              (if replace v3:+zero+ (v::current state))
              vec)
       (when instant
         (v3:copy! (v::previous state) (v::current state))))
      (:model (error "SCALE not yet implemented for world space.")))))

(defun %scale/velocity (transform axis rate)
  (let ((state (scale transform)))
    (setf (v::incremental state) (o:make-velocity axis rate))))

(defun %transform-point (transform point space)
  (let ((model (model transform)))
    (v3:with-components ((v point))
      (~:.xyz
       (ecase space
         (:local (m4:*v4 model (v4:vec vx vy vz 1)))
         (:model (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun %transform-vector (transform vector space)
  (let ((model (m4:copy (model transform))))
    (v3:with-components ((v vector))
      (m4:set-translation! model model v3:+zero+)
      (~:.xyz
       (ecase space
         (:local (m4:*v4 model (v4:vec vx vy vz 1)))
         (:model (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun %transform-direction (transform direction space)
  (let ((model (m4:copy (model transform))))
    (v3:with-components ((v direction))
      (m4:set-translation! model model v3:+zero+)
      (m4:normalize-rotation! model model)
      (~:.xyz
       (ecase space
         (:local (m4:*v4 model (v4:vec vx vy vz 1)))
         (:model (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

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

(defun %scale-around (target-transform pivot-in-world-space scale-diff
                      &key (min-scale (v3:vec 0f0 0f0 0f0))
                        (max-scale
                         (v3:vec most-positive-single-float
                                 most-positive-single-float
                                 most-positive-single-float)))

  (let* (;; We need the translation of the target transform from its parent.
         (a (%get-translation target-transform t))
         ;; We need the pivot point in the coordinte space of the _parent_
         ;; of the target.
         (parent-transform (parent target-transform))
         (b (%transform-point parent-transform pivot-in-world-space :model))
         ;; And the vector from the pivot point to the target in the parent's
         ;; coordinate frame.
         (c (v3:- a b))
         ;; Get the local scale of the target
         (s (%get-scale target-transform t))
         ;; And what that scale would be if we performed the scale-diff.
         (asd (v3:+ s scale-diff)))

    ;; If any component of the scale dips down below or above the extents,
    ;; don't attempt to scale. There are different choices in how this can
    ;; be enforced, for now this is probably good enough.
    (v3:with-components ((asd asd) (mins min-scale) (maxs max-scale))
      (unless (or (< asdx minsx) ;; TODO: Need any< and any> in origin.
                  (< asdy minsy)
                  (< asdz minsz)
                  (> asdx maxsx)
                  (> asdy maxsy)
                  (> asdz maxsz))

        (let* (;; Compute the relative scale vector
               (rs (v3:/ asd s))
               ;; And then use it to compute where the new location position
               ;; is going to be for the target frame.
               (scaled-c (v3:* c rs))
               (fp (v3:+ b scaled-c)))

          ;; Now scale up or down the target's frame by the requested amount.
          (%scale target-transform scale-diff :local nil nil)
          ;; And adjust the target's translation vector from the parent to
          ;; put the pivot point into the same place as the target scales.
          (%translate target-transform fp :local t nil))))))
