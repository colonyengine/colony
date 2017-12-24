(in-package :fl.comp.transform)

(define-component transform ()
  (parent nil)
  (children nil)
  (translation (%make-transform-state 'transform-state-vector))
  (rotation (%make-transform-state 'transform-state-quaternion
                                   :incremental (v3zero)))
  (scale (%make-transform-state 'transform-state-vector
                                :current (vec3 1 1 1)))
  (local (mid))
  (model (mid)))

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
    (v3cp! %previous %current)
    (v3+! %current %current %incremental)))

(defun rotate-node (node)
  (with-slots (%current %incremental %previous) (rotation node)
    (qcp! %previous %current)
    (qrot! %current %current %incremental)))

(defun scale-node (node)
  (with-slots (%current %incremental %previous) (scale node)
    (v3cp! %previous %current)
    (v3+! %current %current %incremental)))

(defun transform-node (node)
  (scale-node node)
  (rotate-node node)
  (translate-node node))

(defun resolve-local (node alpha)
  (with-slots (%scale %rotation %translation %local) node
    (interpolate-state %scale alpha)
    (interpolate-state %rotation alpha)
    (interpolate-state %translation alpha)
    (m*! %local
         (q->m! %local (interpolated %rotation))
         (v3->mscale +mid+ (interpolated %scale)))
    (v3->mtr! %local (interpolated %translation))))

(defun resolve-model (node alpha)
  (with-slots (%parent %local %model) node
    (when %parent
      (resolve-local node alpha)
      (m*! %model (model %parent) %local)
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
                                    (translation/current (v3zero))
                                    (translation/incremental (v3zero))
                                    (rotation/current (v3zero))
                                    (rotation/incremental (v3zero))
                                    (scale/current (vec3 1 1 1))
                                    (scale/incremental (v3zero)))
  (with-slots (%translation %rotation %scale) instance
    (setf (actor instance) actor
          (state instance) :initialize
          (current %translation) translation/current
          (incremental %translation) translation/incremental
          (current %rotation) (qrot +qid+ rotation/current)
          (incremental %rotation) rotation/incremental
          (current %scale) scale/current
          (incremental %scale) scale/incremental)))
