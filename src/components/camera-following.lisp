(in-package :fl.comp.following-camera)

(define-component following-camera ()
  ((slave-camera :default nil)
   (target-actor :default nil)
   (target-transform :default nil)
   (offset :default (v3:zero))))

(defmethod initialize-component ((component following-camera) (context context))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor)) component
    (setf slave (actor-component-by-type actor 'camera))
    (camera-target-actor component target)))

(defmethod update-component ((component following-camera) (context context))
  (with-accessors ((view view) (transform transform)) (slave-camera component)
    (let* ((target-position (m4:translation-to-vec3 (model (target-transform component))))
           (new-camera-position (v3:+! target-position target-position (offset component))))
      (m4:translation-from-vec3! (model transform) new-camera-position)
      (compute-camera-view (slave-camera component) context))))

(defmethod camera-target-actor ((camera following-camera) (actor actor))
  (setf (target-actor camera) actor)
  (when actor
    (setf (target-transform camera) (actor-component-by-type actor 'transform))))
