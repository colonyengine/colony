(in-package :fl.comp.tracking-camera)

(define-component tracking-camera ()
  (slave-camera :default nil)
  (target-actor :default nil)
  (target-transform :default nil))

(defmethod initialize-component ((component tracking-camera) (context context))
  (with-accessors ((slave slave-camera) (actor actor) (target target-actor)) component
    (setf slave (actor-component-by-type actor 'camera))
    (camera-target-actor component target)))

(defmethod update-component ((component tracking-camera) (context context))
  (with-accessors ((view view) (transform transform)) (slave-camera component)
    (let* ((eye (m4:translation-to-vec3 (model transform)))
           (target (m4:translation-to-vec3 (model (target-transform component))))
           (up (v3:make 0.0 1.0 0.0)))
      (m4:view! view eye target up))))

(defmethod camera-target-actor ((camera tracking-camera) (actor actor))
  (setf (target-actor camera) actor)
  (when actor
    (setf (target-transform camera)
          (actor-component-by-type actor 'transform))))
