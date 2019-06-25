(in-package #:first-light.components)

(define-component tracking-camera ()
  ((slave-camera :default nil)
   (target-actor :default nil)
   (target-transform :default nil)))

(defmethod on-component-initialize ((self tracking-camera))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor))
      self
    (setf slave (actor-component-by-type actor 'camera))
    (camera-target-actor self target)))

(defmethod on-component-update ((self tracking-camera))
  (with-accessors ((view view) (transform transform)) (slave-camera self)
    (let* ((model (model transform))
           (eye (m4:get-translation model))
           (target (m4:get-translation (model (target-transform self))))
           (up (v3:make 0 1 0)))
      (m4:set-view! view eye target up))))

(defmethod camera-target-actor ((camera tracking-camera) actor)
  (with-accessors ((target-actor target-actor)
                   (target-transform target-transform))
      camera
    (setf target-actor actor)
    (when actor
      (setf target-transform (actor-component-by-type actor 'transform)))))
