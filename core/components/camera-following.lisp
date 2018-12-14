(in-package :first-light.components)

(define-component following-camera ()
  ((slave-camera :default nil)
   (target-actor :default nil)
   (target-transform :default nil)
   (offset :default (flm:vec3))))

(defmethod initialize-component ((component following-camera))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor)) component
    (setf slave (actor-component-by-type actor 'camera))
    (camera-target-actor component target)))

(defmethod update-component ((component following-camera))
  (with-accessors ((view view) (transform transform)) (slave-camera component)
    (let* ((target-position (flm:get-translation (model (target-transform component))))
           (new-camera-position (flm:+ target-position (offset component) target-position)))
      (flm:set-translation (model transform) new-camera-position (model transform))
      (compute-camera-view (slave-camera component) (context component)))))

(defmethod camera-target-actor ((camera following-camera) (actor actor))
  (setf (target-actor camera) actor)
  (when actor
    (setf (target-transform camera) (actor-component-by-type actor 'transform))))
