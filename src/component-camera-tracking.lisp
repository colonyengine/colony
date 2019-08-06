(in-package #:virality.components.camera.tracking)

(v:define-component tracking-camera ()
  ((%slave :reader slave)
   (%target-actor :accessor target-actor
                  :initarg :target-actor)
   (%target-transform :reader target-transform)))

(defmethod camera-target-actor ((camera tracking-camera) actor)
  (with-slots (%target-transform) camera
    (setf (target-actor camera) actor)
    (when actor
      (setf %target-transform (v:component-by-type actor 'c/xform:transform)))))

(defmethod v:on-component-initialize ((self tracking-camera))
  (with-slots (%slave) self
    (setf %slave (v:component-by-type (v:actor self) 'c/cam:camera))
    (camera-target-actor self (target-actor self))))

(defmethod v:on-component-update ((self tracking-camera))
  (let* ((slave (slave self))
         (model (c/xform:model (c/cam:transform slave)))
         (eye (m4:get-translation model))
         (target (m4:get-translation (c/xform:model (target-transform self))))
         (up (v3:vec 0 1 0)))
    (m4:set-view! (c/cam:view slave) eye target up)))
