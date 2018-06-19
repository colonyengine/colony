(in-package :fl.comp)

(define-component mesh-renderer ()
  ((mesh :default nil)
   (transform :default nil)
   (material :default nil)))

(defmethod initialize-component ((component mesh-renderer) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (with-accessors ((actor actor) (mesh mesh) (transform transform) (material material)) component
      (setf mesh (actor-component-by-type actor 'mesh)
            transform (actor-component-by-type actor 'transform))
      ;; Convert the material from the initial name to a real instance.
      ;; TODO: We specify the material as a symbol in the scene file, but we convert it to a real
      ;; material instance here by looking it up in the context. So, is this a good idea? Or, do we
      ;; keep the name in this slot and call lookup-material when we need to? Should it be more
      ;; automatic? Who would do it? I can't encode the lookup-material into the scene dsl because
      ;; materials don't exist properly until shader programs are made, and that's after scene load.
      (setf material (lookup-material material context)))))

(defmethod render-component ((component mesh-renderer) (context context))
  (with-accessors ((transform transform) (mesh mesh) (material material)) component
    (with-accessors ((draw-mesh draw)) mesh
      (au:when-let ((camera (active-camera context)))
        (using-material material
            (:model (fl.comp:model transform)
             :view (fl.comp:view camera)
             :proj (fl.comp:projection camera))
          (draw-mesh mesh))))))
