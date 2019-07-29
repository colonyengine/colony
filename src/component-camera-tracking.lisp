(in-package #:first-light.components)

(define-component tracking-camera ()
  ((%slave :reader slave)
   (%target-actor :accessor target-actor
                  :initarg :target-actor)
   (%target-transform :reader target-transform)))

(defmethod on-component-initialize ((self tracking-camera))
  (with-slots (%slave) self
    (setf %slave (actor-component-by-type (actor self) 'camera))
    (camera-target-actor self (target-actor self))))

(defmethod on-component-update ((self tracking-camera))
  (let* ((slave (slave self))
         (model (model (transform slave)))
         (eye (m4:get-translation model))
         (target (m4:get-translation (model (target-transform self))))
         (up (v3:vec 0 1 0)))
    (m4:set-view! (view slave) eye target up)))

(defmethod camera-target-actor ((camera tracking-camera) actor)
  (with-slots (%target-transform) camera
    (setf (target-actor camera) actor)
    (when actor
      (setf %target-transform (actor-component-by-type actor 'transform)))))
