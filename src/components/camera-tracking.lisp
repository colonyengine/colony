(in-package #:colony.component)

(c:define-component tracking-camera ()
  ((%slave :reader slave)
   (%target-actor :accessor target-actor
                  :initarg :target-actor)
   (%target-transform :reader target-transform)))

(defmethod camera-target-actor ((camera tracking-camera) actor)
  (with-slots (%target-transform) camera
    (setf (target-actor camera) actor)
    (when actor
      (setf %target-transform (c:component-by-type actor 'transform)))))

(defmethod c:on-component-initialize ((self tracking-camera))
  (with-slots (%slave) self
    (setf %slave (c:component-by-type (c:actor self) 'camera))
    (camera-target-actor self (target-actor self))))

(defmethod c:on-component-update ((self tracking-camera))
  (let* ((slave (slave self))
         (model (c:get-model-matrix (transform slave)))
         (eye (m4:get-translation model))
         (target (m4:get-translation (c:get-model-matrix
                                      (target-transform self))))
         (up (v3:vec 0f0 1f0 0f0)))
    (m4:look-at! (view slave) eye target up)))
