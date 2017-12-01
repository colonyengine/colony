(in-package :first-light)

(define-component mesh-renderer ()
  (mesh nil)
  (transform nil)
  (shader :default))

(defmethod initialize-component ((component mesh-renderer) (context context))
  (with-accessors ((actor actor) (mesh mesh) (transform transform)) component
    (setf mesh (actor-component-by-type actor 'mesh)
          transform (actor-component-by-type actor 'transform))))

(defmethod render-component ((component mesh-renderer) (context context))
  (with-accessors ((transform transform) (mesh mesh)) component
    (when-let* ((shaders (shaders context))
                (camera (camera context)))
      (kit.gl.shader:use-program shaders (shader component))
      (uniform-matrix shaders :model (model transform))
      (uniform-matrix shaders :view (view camera))
      (uniform-matrix shaders :proj (projection camera))
      (kit.gl.vao:vao-draw (vao mesh)))))

(defun uniform-matrix (shaders key value)
  (kit.gl.shader:uniform-matrix-1-sv shaders key value))

(defun uniform-vector (shaders key value)
  (kit.gl.shader:uniformfv shaders key value))

(defun uniform-integer (shaders key value)
  (kit.gl.shader:uniformi shaders key value))

(defun uniform-float (shaders key value)
  (kit.gl.shader:uniformf shaders key value))
