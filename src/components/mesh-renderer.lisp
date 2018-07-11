(in-package :fl.comp)

(define-component mesh-renderer ()
  ((mesh :default nil)
   (transform :default nil)
   (material :default nil
             :annotation (fl.annotations:material))))

(defmethod initialize-component ((component mesh-renderer) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (with-accessors ((actor actor) (mesh mesh) (transform transform) (material material)) component
      (setf mesh (actor-component-by-type actor 'mesh)
            transform (actor-component-by-type actor 'transform)))))

(defmethod render-component ((component mesh-renderer) (context context))
  (with-accessors ((transform transform) (mesh mesh) (material material)) component
    (with-accessors ((draw-mesh draw)) mesh
      (au:when-let ((camera (active-camera context)))
        (using-material material
            (:model (fl.comp:model transform)
             :view (fl.comp:view camera)
             :proj (fl.comp:projection camera))
          (draw-mesh mesh))))))
