(in-package #:first-light.components)

(define-component following-camera ()
  ((%slave :reader slave)
   (%target-actor :accessor target-actor
                  :initarg :target-actor)
   (%target-transform :reader target-transform)
   (%offset :reader offset
            :initarg :offset
            :initform (v3:zero))))

(defmethod on-component-initialize ((self following-camera))
  (with-slots (%slave) self
    (setf %slave (actor-component-by-type (actor self) 'camera))
    (camera-target-actor self (target-actor self))))

(defmethod on-component-update ((self following-camera))
  (with-slots (%transform) (slave self)
    (let* ((target-position (m4:get-translation
                             (model (target-transform self))))
           (new-camera-position (v3:+! target-position
                                       target-position
                                       (offset self))))
      (m4:set-translation! (model %transform)
                           (model %transform)
                           new-camera-position)
      (compute-camera-view (slave self)))))

(defmethod camera-target-actor ((camera following-camera) actor)
  (with-slots (%target-transform) camera
    (setf (target-actor camera) actor)
    (when actor
      (setf %target-transform (actor-component-by-type actor 'transform)))))
