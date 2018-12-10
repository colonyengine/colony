(in-package :fl.comp)

(define-component transform ()
  ((parent :default nil)
   (children :default nil)
   (translation :default (make-transform-state 'transform-state-vector
                                               :incremental-delta (flm:vec3)))
   (rotation :default (make-transform-state 'transform-state-quaternion
                                            :incremental (flm:vec3)
                                            :incremental-delta (flm:vec3)))
   (scaling :default (make-transform-state 'transform-state-vector
                                           :current (flm:vec3 1)
                                           :incremental-delta (flm:vec3)))
   (local :default (flm:mat4 1))
   (model :default (flm:mat4 1))))

(defun add-child (parent child)
  (push child (children parent))
  (setf (parent child) parent))

(defun remove-child (parent child)
  (setf (children parent) (remove-if (lambda (c) (eq c child)) (children parent))
        (parent child) nil))

(defun translate-node (node delta)
  (with-accessors ((c current) (i incremental) (idelta incremental-delta) (p previous))
      (translation node)
    (flm:copy-into p c)
    (flm:+ c (flm:* i delta idelta) c)))

(defun rotate-node (node delta)
  (with-accessors ((c current) (i incremental) (idelta incremental-delta) (p previous))
      (rotation node)
    (flm:copy-into p c)
    (flm:rotate c (flm:* i delta idelta) c)))

(defun scale-node (node delta)
  (with-accessors ((c current) (i incremental) (idelta incremental-delta) (p previous))
      (scaling node)
    (flm:copy-into p c)
    (flm:+ c (flm:* i delta idelta) c)))

(defun transform-node (core-state node)
  (let ((delta (delta (context core-state))))
    (scale-node node delta)
    (rotate-node node delta)
    (translate-node node delta)))

(defun resolve-local (node alpha)
  (with-slots (%scaling %rotation %translation %local) node
    (interpolate-state %scaling alpha)
    (interpolate-state %rotation alpha)
    (interpolate-state %translation alpha)
    (flm:* (flm:mat4 (interpolated %rotation) %local)
           (flm:set-scale flm:+id-mat4+ (interpolated %scaling))
           %local)
    (flm:set-translation %local (interpolated %translation) %local)))

(defun resolve-model (node alpha)
  (with-slots (%parent %local %model) node
    (when %parent
      (resolve-local node alpha)
      (flm:* (model %parent) %local %model))))

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
  (let ((instance (make-instance component-type :type component-type :context context)))
    (apply #'reinitialize-instance instance :type component-type :context context args)
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

          (current %rotation)
          ;; TODO: Maybe should make this conversion into a GF or something.
          (etypecase rotation/current
            (flm:vec3
             ;; euler angles being passed in.
             (flm:rotate flm:+id-quat+ rotation/current))
            (flm:quat
             ;; quaternion being passed in: use as is.
             rotation/current))

          (previous %rotation) (flm:copy (current %rotation))
          (incremental %rotation) rotation/incremental

          (current %scaling) scale/current
          (previous %scaling) (flm:copy scale/current)
          (incremental %scaling) scale/incremental)))

;;; User protocol

(defun %rotate/model-space (rotation vec &optional replace-p)
  (with-accessors ((current current)) rotation
    (flm:rotate (if replace-p flm:+id-quat+ current) vec current)))

(defun %rotate/world-space (rotation vec &optional replace-p)
  (declare (ignore rotation vec replace-p))
  (error "ROTATE not yet implemented for world space."))

(defun rotate (transform vec &key (space :model) replace-p)
  (ecase space
    (:model (%rotate/model-space (rotation transform) vec replace-p))
    (:world (%rotate/world-space (rotation transform) vec replace-p))))

(defun %translate/model-space (translation vec &optional replace-p)
  (with-accessors ((current current)) translation
    (flm:+ (if replace-p flm:+zero-vec3+ current) vec current)))

(defun %translate/world-space (translation vec &optional replace-p)
  (declare (ignore translation vec replace-p))
  (error "TRANSLATE not yet implemented for world space."))

(defun translate (transform vec &key (space :model) replace-p)
  (ecase space
    (:model (%translate/model-space (translation transform) vec replace-p))
    (:world (%translate/world-space (translation transform) vec replace-p))))

(defun scale (transform vec &key replace-p)
  (with-accessors ((current current)) (scaling transform)
    (flm:+ (if replace-p flm:+zero-vec3+ current) vec current)))
