(in-package :first-light.components)

(define-component tracking-camera ()
  ((slave-camera :default nil)
   (target-actor :default nil)
   (target-transform :default nil)))

(defmethod on-component-initialize ((component tracking-camera))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor)) component
    (setf slave (actor-component-by-type actor 'camera))
    (camera-target-actor component target)))

(defmethod on-component-update ((component tracking-camera))
  (with-accessors ((view view) (transform transform)) (slave-camera component)
    (let* ((model (model transform))
           (eye (flm:get-translation model))
           (target (flm:get-translation (model (target-transform component))))
           (up (flm:vec3 0 1 0)))
      (flm:set-view eye target up view))))

(defmethod camera-target-actor ((camera tracking-camera) (actor actor))
  (setf (target-actor camera) actor)
  (when actor
    (setf (target-transform camera) (actor-component-by-type actor 'transform))))
