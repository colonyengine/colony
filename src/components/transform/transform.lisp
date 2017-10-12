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


;; NOTE: Each component requires a make-TYPE function for it.
;; This one probably needs more work. :)
(defun make-transform (&key
                         (game-object NIL)
                         (translation/current (vec))
                         (translation/incremental (vec))
                         (rotation/current (vec))
                         (rotation/incremental (vec))
                         (scale/current (vec 1 1 1))
                         (scale/incremental (vec)))

  (make-instance 'transform
                 :game-object game-object
                 :translation (%make-transform-state
                               'transform-state-vector
                               :current translation/current
                               :incremental translation/incremental)
                 :rotation (%make-transform-state
                            'transform-state-quaternion
                            :current rotation/current
                            :incremental rotation/incremental)
                 :scale (%make-transform-state
                         'transform-state-vector
                         :current scale/current
                         :incremental scale/incremental)))
