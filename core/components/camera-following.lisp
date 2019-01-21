(in-package :first-light.components)

(define-component following-camera ()
  ((slave-camera :default nil)
   (target-actor :default nil)
   (target-transform :default nil)
   (offset :default (m:vec3))))

(defmethod on-component-initialize ((self following-camera))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor)) self
    (setf slave (actor-component-by-type actor 'camera))
    (camera-target-actor self target)))

(defmethod on-component-update ((self following-camera))
  (with-accessors ((view view) (transform transform)) (slave-camera self)
    (let* ((target-position (m:get-translation (model (target-transform self))))
           (new-camera-position (m:+ target-position (offset self) target-position)))
      (m:set-translation (model transform) new-camera-position (model transform))
      (compute-camera-view (slave-camera self)))))

(defmethod camera-target-actor ((camera following-camera) actor)
  (with-accessors ((target-actor target-actor) (target-transform target-transform)) camera
    (setf target-actor actor)
    (when actor
      (setf target-transform (actor-component-by-type actor 'transform)))))
