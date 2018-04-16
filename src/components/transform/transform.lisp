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
  (setf (children parent)
        (remove-if (lambda (c) (eq c child))
                   (children parent)))
  (setf (parent child) nil))

(defun translate-node (node)
  (with-slots (%current %incremental %previous) (translation node)
    (v3:copy! %previous %current)
    (v3:+! %current %current %incremental)))

(defun rotate-node (node)
  (with-slots (%current %incremental %previous) (rotation node)
    (q:copy! %previous %current)
    (q:rotate! %current %current %incremental)))

(defun scale-node (node)
  (with-slots (%current %incremental %previous) (scale node)
    (v3:copy! %previous %current)
    (v3:+! %current %current %incremental)))

(defun transform-node (node)
  (scale-node node)
  (rotate-node node)
  (translate-node node))

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
  ;; TODO: Do I have to qualify type here?
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
