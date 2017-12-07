(in-package :fl.comp.following-camera)

;;; For this component, the camera transform represents the vector away from the
;;; target that the camera sits. The following-camera does NOT rotate with the
;;; target, it only follows it from an offset. So it is not the same operation
;;; as parent the camera to the target.

(define-component following-camera (target-camera)
  (offset (v3zero)))

(defmethod update-component ((component following-camera) (context context))
  (with-accessors ((view view) (transform transform)) (slave-camera component)
    (let* ((target-position (mtr->v3 (model (target-transform component))))
           (new-camera-position (v3+! target-position
                                      target-position
                                      (offset component))))
      (v3->mtr! (model transform) new-camera-position)
      (compute-camera-view (slave-camera component) context))))
