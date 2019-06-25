(in-package #:first-light.components)

(define-component following-camera ()
  ((slave-camera :default nil)
   (target-actor :default nil)
   (target-transform :default nil)
   (offset :default (v3:zero))))

(defmethod on-component-initialize ((self following-camera))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor))
      self
    (setf slave (actor-component-by-type actor 'camera))
    (camera-target-actor self target)))

(defmethod on-component-update ((self following-camera))
  (with-accessors ((view view) (transform transform)) (slave-camera self)
    (let* ((target-position (m4:get-translation
                             (model (target-transform self))))
           (new-camera-position (v3:+! target-position
                                       target-position
                                       (offset self))))
      (m4:set-translation! (model transform)
                           (model transform)
                           new-camera-position)
      (compute-camera-view (slave-camera self)))))

(defmethod camera-target-actor ((camera following-camera) actor)
  (with-accessors ((target-actor target-actor)
                   (target-transform target-transform))
      camera
    (setf target-actor actor)
    (when actor
      (setf target-transform (actor-component-by-type actor 'transform)))))
