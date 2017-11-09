(in-package :gear)

(define-component tracking-camera (camera)
  (look-at-actor nil)
  (look-at-transform nil))

(defmethod initialize-component :around ((component tracking-camera)
                                         (context context))
  (call-next-method)
  (look-at component (look-at-actor component)))

(defmethod compute-camera-view ((camera tracking-camera) (context context))
  (with-accessors ((view view) (transform transform) (look-at look-at-transform))
      camera
    (let* ((eye (mtr->v (model transform)))
           (target (mtr->v (model look-at)))
           (up (vec 0 1 0)))
      (mkview! view eye target up))))

(defun look-at (camera actor)
  "Set LOOK-AT-ACTOR into the CAMERA-COMPONENT and make the camera
track that actor. To unset, set LOOK-AT-ACTOR to NIL when calling
function. That will restore the camera to its default configuration of
matching the associated actor's transform for its orientation."
  (setf (look-at-actor camera) actor)
  (when actor
    (setf (look-at-transform camera) (get-component 'transform actor))))
