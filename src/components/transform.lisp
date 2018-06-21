(in-package :fl.comp)

(define-component transform ()
  ((parent :default nil)
   (children :default nil)
   (translation :default (make-transform-state 'transform-state-vector
                                               :incremental-delta (v3:zero)))
   (rotation :default (make-transform-state 'transform-state-quaternion
                                            :incremental (v3:zero)
                                            :incremental-delta (v3:zero)))
   (scaling :default (make-transform-state 'transform-state-vector
                                           :current (v3:one)
                                           :incremental-delta (v3:zero)))
   (local :default (m4:id))
   (model :default (m4:id))))

(defun add-child (parent child)
  (push child (children parent))
  (setf (parent child) parent))

(defun remove-child (parent child)
  (setf (children parent) (remove-if (lambda (c) (eq c child)) (children parent))
        (parent child) nil))

(defun translate-node (node delta)
  (with-accessors ((c current) (i incremental) (idelta incremental-delta) (p previous))
      (translation node)
    (v3:copy! p c)
    (v3:+! c c (v3:scale! idelta i delta))))

(defun rotate-node (node delta)
  (with-accessors ((c current) (i incremental) (idelta incremental-delta) (p previous))
      (rotation node)
    (q:copy! p c)
    (q:rotate! c c (v3:scale! idelta i delta))))

(defun scale-node (node delta)
  (with-accessors ((c current) (i incremental) (idelta incremental-delta) (p previous))
      (scaling node)
    (v3:copy! p c)
    (v3:+! c c (v3:scale! idelta i delta))))

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
    (m4:*! %local
           (q:to-mat4! %local (interpolated %rotation))
           (m4:scale-from-vec3 m4:+id+ (interpolated %scaling)))
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
  (let ((instance (make-instance component-type :type component-type :context context)))
    (apply #'reinitialize-instance instance :type component-type :context context args)
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
  (with-slots (%translation %rotation %scaling) instance
    (setf (actor instance) actor
          (state instance) :initialize
          (current %translation) translation/current
          (incremental %translation) translation/incremental
          (current %rotation) (q:rotate q:+id+ rotation/current)
          (incremental %rotation) rotation/incremental
          (current %scaling) scale/current
          (incremental %scaling) scale/incremental)))

;;; User protocol

(defun %rotate/model-space (rotation vec &optional replace-p)
  (with-accessors ((current current)) rotation
    (q:rotate! current (if replace-p q:+id+ current) vec)))

(defun %rotate/world-space (rotation vec &optional replace-p)
  (declare (ignore rotation vec replace-p))
  (error "ROTATE not yet implemented for world space."))

(defun rotate (transform vec &key (space :model) replace-p)
  (ecase space
    (:model (%rotate/model-space (rotation transform) vec replace-p))
    (:world (%rotate/world-space (rotation transform) vec replace-p))))

(defun %translate/model-space (translation vec &optional replace-p)
  (with-accessors ((current current)) translation
    (v3:+! current (if replace-p v3:+zero+ current) vec)))

(defun %translate/world-space (translation vec &optional replace-p)
  (declare (ignore translation vec replace-p))
  (error "TRANSLATE not yet implemented for world space."))

(defun translate (transform vec &key (space :model) replace-p)
  (ecase space
    (:model (%translate/model-space (translation transform) vec replace-p))
    (:world (%translate/world-space (translation transform) vec replace-p))))

(defun scale (transform vec &key replace-p)
  (with-accessors ((current current)) (scaling transform)
    (v3:+! current (if replace-p v3:+zero+ current) vec)))
