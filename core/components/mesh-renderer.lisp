(in-package :first-light.components)

(define-component mesh-renderer ()
  ((mesh :default nil)
   (transform :default nil)
   (material :default nil
             :annotation (fl.annotations:material))))

(defmethod on-component-initialize ((self mesh-renderer))
  (with-accessors ((actor actor) (mesh mesh) (transform transform) (material material)) self
    (setf mesh (actor-component-by-type actor 'mesh)
          transform (actor-component-by-type actor 'transform))))

(defmethod on-component-render ((self mesh-renderer))
  (with-accessors ((context context) (transform transform) (mesh mesh) (material material)) self
    (with-accessors ((draw-mesh draw)) mesh
      (fl.util:when-let ((camera (active-camera context)))
        (using-material material
            (:model (fl.comp:model transform)
             :view (fl.comp:view camera)
             :proj (fl.comp:projection camera))
          (draw-mesh mesh :instance-count (instances material)))))))
