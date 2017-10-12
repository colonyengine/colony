(in-package :gear)

(defclass transformable ()
  ((%translation :accessor translation
                 :initarg :translation
                 :initform (%make-transform-state 'transform-state-vector))
   (%rotation :accessor rotation
              :initarg :rotation
              :initform (%make-transform-state 'transform-state-quaternion
                                               :incremental (vec)))
   (%scale :accessor scale
           :initarg :scale
           :initform (%make-transform-state 'transform-state-vector))
   (%local :accessor local
           :initarg :local
           :initform (mid))
   (%model :accessor model
           :initarg :model
           :initform (mid))))

(defclass transform (component transformable)
  ((%parent :accessor parent
            :initarg :parent
            :initform nil)
   (%children :accessor children
              :initarg :children
              :initform nil)))

(defgeneric add-child (parent child)
  (:method ((parent transform) (child transform))
    (push child (children parent))
    (setf (parent child) parent)))

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

;; TODO: change to reflect engine idea spec'd out by psilord
(defun node-modified-p (node)
  (when node
    (or (modifiedp (translation node))
        (modifiedp (rotation node))
        (modifiedp (scale node)))))

;; TODO: change to reflect engine idea spec'd out by psilord
(defgeneric transform-node (node)
  (:method (node))
  (:method ((node transformable))
    (scale-node node)
    (rotate-node node)
    (translate-node node)))

;; TODO: change to reflect engine idea spec'd out by psilord
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

;; TODO: change to reflect engine idea spec'd out by psilord
(defun resolve-model (node alpha)
  (with-slots (%parent %local %model) node
    (when %parent
      (resolve-local node alpha)
      (m*! %model (model %parent) %local)
      %model)))

;; TODO: change to reflect engine idea spec'd out by psilord
(defun do-nodes (effect)
  (declare (ignore effect)))

;; TODO: change to reflect engine idea spec'd out by psilord
(defun interpolate-transforms (alpha)
  (do-nodes (lambda (node) (resolve-model node alpha))))

;; Need to reformat arguments to be appropriate, so provide a specific
;; entry here for this type.
(defmethod make-component ((comp-type (eql 'transform)) &rest args)
  (let ((inst (make-instance 'transform)))
    (apply #'reinitialize-instance inst args)
    inst))

;; NOTE: We do this because reinitialize-instance for a transform needs to
;; process its argument before actually reinitializing the instance of the
;; transform with the new data.
(defmethod reinitialize-instance ((inst transform) &rest args)
  (apply #'reinitialize-transform-instance inst args))

(defun reinitialize-transform-instance (instance
                                        &key
                                          (game-object nil p/0)
                                          (translation/current (vec) p/1)
                                          (translation/incremental (vec) p/2)
                                          (rotation/current (vec) p/3)
                                          (rotation/incremental (vec) p/4)
                                          (scale/current (vec 1 1 1) p/5)
                                          (scale/incremental (vec) p/6))

  (when p/0
    (setf (game-object instance) game-object))

  (when p/1
    (setf (current (translation instance)) translation/current))

  (when p/2
    (setf (incremental (translation instance)) translation/incremental))

  (when p/3
    (setf (current (rotation instance)) rotation/current))

  (when p/4
    (setf (incremental (rotation instance)) rotation/incremental))

  (when p/5
    (setf (current (scale instance)) scale/current))

  (when p/6
    (setf (incremental (scale instance)) scale/incremental)))
