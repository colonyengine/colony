(in-package :fl.comp.following-camera)

;;; For this component, the camera transform represents the vector away from the
;;; target that the camera sits. The following-camera does NOT rotate with the
;;; target, it only follows it from an offset. So it is not the same operation
;;; as parent the camera to the target.

(define-component following-camera ()
  (slave-camera nil)
  (offset (vec 0 0 0))
  (target-actor nil)
  (target-transform nil))

(defun target-actor-with-following-camera (following-camera actor)
  (setf (target-actor following-camera) actor)
  (when actor
    (setf (target-transform following-camera)
          (actor-component-by-type actor 'transform))))

(defmethod initialize-component ((component following-camera)
                                 (context context))
  (setf (slave-camera component) (actor-component-by-type
                                  (actor component) 'camera))
  (target-actor-with-following-camera component (target-actor component)))

(defmethod update-component ((component following-camera) (context context))
  (with-accessors ((view view) (transform transform))
      (slave-camera component)
    ;; First, let's move the camera, by the camera's translation/current awy
    ;; from the target. We specifically do not affect its rotation.
    (let* ((target-position (mtr->v (model (target-transform component))))
           (new-camera-position (v+! target-position
                                     target-position
                                     (offset component))))
      ;; Then, fix the transform of the camera to represent this new situation.
      (v->mtr! (model transform) new-camera-position)
      ;; Finally, recompute the view matrix.
      (compute-camera-view (slave-camera component) context))))
