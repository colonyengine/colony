(in-package :first-light)

(define-component mesh-renderer ()
  (mesh nil)
  (transform nil))

(defmethod initialize-component ((component mesh-renderer) (context context))
  (with-accessors ((actor actor) (mesh mesh) (transform transform)) component
    (setf mesh (actor-component-by-type actor 'mesh)
          transform (actor-component-by-type actor 'transform))))

(defmethod render-component ((component mesh-renderer) (context context))
  (let* ((shaders (shaders context))
         (camera (camera context)))
    (when (and shaders camera)
      (let ((model (model (transform component)))
            (view (view camera))
            (projection (projection camera)))
        (kit.gl.shader:use-program shaders :unlit-texture)
        (kit.gl.shader:uniform-matrix-1-sv shaders :model model)
        (kit.gl.shader:uniform-matrix-1-sv shaders :view view)
        (kit.gl.shader:uniform-matrix-1-sv shaders :proj projection)
        (kit.gl.vao:vao-draw (vao (mesh component)))))))
