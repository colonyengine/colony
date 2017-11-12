(in-package :first-light)

(define-component transform ()
  (parent nil)
  (children nil)
  (translation (%make-transform-state 'transform-state-vector))
  (rotation (%make-transform-state 'transform-state-quaternion
                                   :incremental (vec)))
  (scale (%make-transform-state 'transform-state-vector
                                :current (vec 1 1 1)))
  (local (mid))
  (model (mid)))

(defun add-child (parent child)
  (push child (children parent))
  (setf (parent child) parent))

(defun translate-node (node)
  (with-slots (%current %incremental %previous) (translation node)
    (vcp! %previous %current)
    (v+! %current %current %incremental)))

(defun rotate-node (node)
  (with-slots (%current %incremental %previous) (rotation node)
    (qcp! %previous %current)
    (qrot! %current %current %incremental)))

(defun scale-node (node)
  (with-slots (%current %incremental %previous) (scale node)
    (vcp! %previous %current)
    (v+! %current %current %incremental)))

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
         (v->mscale +mid+ (interpolated %scale)))
    (v->mtr! %local (interpolated %translation))))

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

(defmethod make-component ((type (eql 'transform)) &rest args)
  (let ((instance (make-instance 'transform)))
    (apply #'reinitialize-instance instance args)
    instance))

(defmethod reinitialize-instance ((instance transform)
                                  &key
                                    actor
                                    (translation/current (vec))
                                    (translation/incremental (vec))
                                    (rotation/current (vec))
                                    (rotation/incremental (vec))
                                    (scale/current (vec 1 1 1))
                                    (scale/incremental (vec)))
  (with-slots (%actor %state %translation %rotation %scale) instance
    (setf %actor actor
          %state :initialize
          (current %translation) translation/current
          (incremental %translation) translation/incremental
          (current %rotation) (qrot +qid+ rotation/current)
          (incremental %rotation) rotation/incremental
          (current %scale) scale/current
          (incremental %scale) scale/incremental)))
