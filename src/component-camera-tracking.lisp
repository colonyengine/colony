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
      (setf %target-transform (v:component-by-type actor 'comp.transform:transform)))))

(defmethod v:on-component-initialize ((self tracking-camera))
  (with-slots (%slave) self
    (setf %slave (v:component-by-type (v:actor self) 'comp.camera:camera))
    (camera-target-actor self (target-actor self))))

(defmethod v:on-component-update ((self tracking-camera))
  (let* ((slave (slave self))
         (model (comp.transform:model (comp.camera:transform slave)))
         (eye (m4:get-translation model))
         (target (m4:get-translation (comp.transform:model (target-transform self))))
         (up (v3:vec 0 1 0)))
    (m4:set-view! (comp.camera:view slave) eye target up)))
