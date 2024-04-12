(in-package #:colony.component)

(c:define-component following-camera ()
  ((%slave :reader slave)
   (%target-actor :accessor target-actor
                  :initarg :target-actor)
   (%target-transform :reader target-transform)
   (%offset :reader offset
            :initarg :offset
            :initform (v3:zero))))

(defmethod camera-target-actor ((camera following-camera) actor)
  (with-slots (%target-transform) camera
    (setf (target-actor camera) actor)
    (when actor
      (setf %target-transform (c:component-by-type actor 'transform)))))

(defmethod c:on-component-initialize ((self following-camera))
  (with-slots (%slave) self
    (setf %slave (c:component-by-type (c:actor self) 'camera))
    (camera-target-actor self (target-actor self))))

(defmethod c:on-component-update ((self following-camera))
  (with-slots (%transform) (slave self)
    (let* ((target-position (m4:get-translation
                             (c:get-model-matrix (target-transform self))))
           (new-camera-position (v3:+! target-position
                                       target-position
                                       (offset self)))
           (model (c:get-model-matrix self)))
      (m4:set-translation! model model new-camera-position)
      (compute-camera-view (slave self)))))
