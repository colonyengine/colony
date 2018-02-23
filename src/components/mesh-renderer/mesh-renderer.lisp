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
  (with-accessors ((transform transform) (mesh mesh) (shader shader)
                   (texture-id texture-id))
      component
    (alexandria:when-let* ((shaders (shaders context))
                           (camera (active-camera context)))

      #++(progn
           (kit.gl.shader:use-program shaders shader)
           (uniform-matrix shaders :model (model transform))
           (uniform-matrix shaders :view (view camera))
           (uniform-matrix shaders :proj (projection camera))
           (when texture-id
             (gl:active-texture 0)
             (gl:bind-texture :texture-2d texture-id)
             (uniform-integer shaders :tex.sampler1 0)))

      (progn
        #++(format t "Attempting to render with shader: ~S (texture-id: ~A)~%"
                   shader texture-id)
        (shadow:with-shader-program shader
          (shadow:uniform-mat4 :model (model transform))
          (shadow:uniform-mat4 :view (view camera))
          (shadow:uniform-mat4 :proj (projection camera))
          (when texture-id
            (gl:active-texture 0)
            (gl:bind-texture :texture-2d texture-id)
            (shadow:uniform-int :tex.sampler1 0))))
      (dolist (primitive (primitives mesh))
        (funcall (fl.assets:draw-func primitive))))))

(defun uniform-matrix (shaders key value)
  (kit.gl.shader:uniform-matrix-1-sv shaders key value))

(defun uniform-vector (shaders key value)
  (kit.gl.shader:uniformfv shaders key value))

(defun uniform-integer (shaders key value)
  (kit.gl.shader:uniformi shaders key value))

(defun uniform-float (shaders key value)
  (kit.gl.shader:uniformf shaders key value))
