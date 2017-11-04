(in-package :gear)

(%define-core-component transform ()
  (parent nil)
  (children nil)
  (translation (%make-transform-state 'transform-state-vector))
  (rotation (%make-transform-state 'transform-state-quaternion
                                   :incremental (vec)))
  (scale (%make-transform-state 'transform-state-vector))
  (local (mid))
  (model (mid)))

(defun add-child (parent child)
  (push child (children parent))
  (setf (parent child) parent))

(defun translate-node (node)
  (with-slots (%current %incremental %previous %modifiedp) (translation node)
    (let ((locally-modified-p (not (vzerop %incremental))))
      (vcp! %previous %current)
      (when locally-modified-p
        (v+! %current %current %incremental))
      (setf %modifiedp locally-modified-p))))

(defun rotate-node (node)
  (with-slots (%current %incremental %previous %modifiedp) (rotation node)
    (let ((locally-modified-p (not (vzerop %incremental))))
      (qcp! %previous %current)
      (when locally-modified-p
        (qrot! %current %current %incremental))
      (setf %modifiedp locally-modified-p))))

(defun scale-node (node)
  (with-slots (%current %incremental %previous %modifiedp) (scale node)
    (let ((locally-modified-p (not (vzerop %incremental))))
      (vcp! %previous %current)
      (when locally-modified-p
        (v+! %current %current %incremental))
      (setf %modifiedp locally-modified-p))))

(defun node-modified-p (node)
  (when node
    (or (modifiedp (translation node))
        (modifiedp (rotation node))
        (modifiedp (scale node)))))

(defun transform-node (node)
  (scale-node node)
  (rotate-node node)
  (translate-node node))

;; Checked by MF. Don't change.
(defun resolve-local (node alpha)
  (with-slots (%scale %rotation %translation %local) node
    (interpolate-state %scale alpha)
    (interpolate-state %rotation alpha)
    (interpolate-state %translation alpha)
    (when (node-modified-p node)
      (m*! %local
           (q->m! %local (interpolated %rotation))
           (v->mscale +mid+ (interpolated %scale)))
      (v->mtr! %local (interpolated %translation)))))

;; Checked by MF. Only change when render system and delta frame management
;; system is in place.
;; TODO: This is meant to be called from INTERPOLATE-TRANSFORMS.
;; TODO: This is always called with an alpha value, which is the interpolation
;; factor.
(defun resolve-model (node &optional (alpha 0.5))
  (with-slots (%parent %local %model) node
    (when %parent
      (resolve-local node alpha)
      (m*! %model (model %parent) %local)
      %model)))

;; Checked by MF. Only change when render system and delta frame management
;; system is in place.
;; TODO: Parent should be optional, and default to the root of the tree
;; (@universe). I think this will need to pass in a core-state instance to
;; retrieve the root node.
(defun do-nodes (func parent)
  (funcall func parent)
  (dolist (child (children parent))
    (do-nodes func child)))

;; Checked by MF. Only change when render system and delta frame management
;; system is in place.
;; TODO: This is called from the render loop, usually at 60Hz. The render loop
;; is responsible for calculating the alpha value to call this with, which will
;; interpolate the physics by that factor.
;; TODO: When DO-NODES accepts an optional parent argument for the root node,
;; remove the NIL below.
(defun interpolate-transforms (scene-root-transform alpha)
  (do-nodes (lambda (node) (resolve-model node alpha)) scene-root-transform))

(defmethod make-component ((type (eql 'transform)) &rest initargs)
  (let ((instance (make-instance 'transform)))
    (apply #'reinitialize-instance instance initargs)
    instance))

(defun reinitialize-transform-instance (instance
                                        &key
                                          (actor nil p/0)
                                          (state :initialize p/1)
                                          (translation/current (vec) p/2)
                                          (translation/incremental (vec) p/3)
                                          (rotation/current (vec) p/4)
                                          (rotation/incremental (vec) p/5)
                                          (scale/current (vec 1 1 1) p/6)
                                          (scale/incremental (vec) p/7))

  (when p/0
    (setf (actor instance) actor))
  (when p/1
    (setf (state instance) state))
  (when p/2
    (setf (current (translation instance)) translation/current))
  (when p/3
    (setf (incremental (translation instance)) translation/incremental))
  (when p/4
    (setf (current (rotation instance)) rotation/current))
  (when p/5
    (setf (incremental (rotation instance)) rotation/incremental))
  (when p/6
    (setf (current (scale instance)) scale/current))
  (when p/7
    (setf (incremental (scale instance)) scale/incremental)))

(defmethod reinitialize-instance ((instance transform) &rest initargs)
  (apply #'reinitialize-transform-instance instance initargs))
