(in-package :fl.comp.tracking-camera)

(define-component tracking-camera ()
  (slave-camera nil)
  (target-actor nil)
  (target-transform nil))

(defmethod initialize-component ((component tracking-camera) (context context))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor))
      component
    (setf slave (actor-component-by-type actor 'camera))
    (camera-target-actor component target)))

(defmethod update-component ((component tracking-camera) (context context))
  (with-accessors ((view view) (transform transform)) (slave-camera component)
    (let* ((eye (mtr->v3 (model transform)))
           (target (mtr->v3 (model (target-transform component))))
           (up (vec3 0 1 0)))
      (mkview! view eye target up))))

(defmethod camera-target-actor ((camera tracking-camera) (actor actor))
  (setf (target-actor camera) actor)
  (when actor
    (setf (target-transform camera)
          (actor-component-by-type actor 'transform))))
