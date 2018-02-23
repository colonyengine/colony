(in-package :fl.comp.mesh-renderer)

(define-component mesh-renderer ()
  (mesh nil)
  (transform nil)
  (shader :default)
  (texture-location nil)
  (texture-id nil))

(defmethod initialize-component ((component mesh-renderer) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (with-accessors ((actor actor) (mesh mesh) (transform transform)
                     (texture-id texture-id) (texture-location texture-location))
        component
      (setf mesh (actor-component-by-type actor 'mesh)
            transform (actor-component-by-type actor 'transform))
      (unless store
        (setf store (make-instance 'mesh-renderer-shared-storage)))
      (when texture-location
        (multiple-value-bind (cached presentp)
            (cached-texture store texture-location)
          (if presentp
              (setf texture-id (id cached))
              (let* ((id (load-texture context texture-location))
                     (cached (make-cached-texture texture-location id)))
                (setf (cached-texture store texture-location) cached
                      texture-id (id cached)))))))))

(defmethod render-component ((component mesh-renderer) (context context))
  (with-accessors ((transform transform) (mesh mesh) (shader shader) (texture-id texture-id))
      component
    (alexandria:when-let* ((camera (active-camera context)))
      (shadow:with-shader-program shader
        (shadow:uniform-mat4 :model (model transform))
        (shadow:uniform-mat4 :view (view camera))
        (shadow:uniform-mat4 :proj (projection camera))
        (when texture-id
          (gl:active-texture 0)
          (gl:bind-texture :texture-2d texture-id)
          (shadow:uniform-int :tex.sampler1 0))
        (dolist (primitive (primitives mesh))
          (funcall (fl.assets:draw-func primitive)))))))
