(in-package :fl.comp.tracking-camera)

(define-component tracking-camera ()
  (slave-camera nil)
  (target-actor nil)
  (target-transform nil))

(defun target-actor-with-tracking-camera (tracking-camera actor)
  (setf (target-actor tracking-camera) actor)
  (when actor
    (setf (target-transform tracking-camera)
          (actor-component-by-type actor 'transform))))

(defmethod initialize-component ((component tracking-camera) (context context))
  (setf (slave-camera component)
        (actor-component-by-type (actor component) 'camera))
  (target-actor-with-tracking-camera component (target-actor component)))

(defmethod update-component ((component tracking-camera) (context context))
  (with-accessors ((view view) (transform transform)) (slave-camera component)
    (let* ((eye (mtr->v (model transform)))
           (target (mtr->v (model (target-transform component))))
           (up (vec 0 1 0)))
      (mkview! view eye target up))))
