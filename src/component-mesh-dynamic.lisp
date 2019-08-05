(in-package #:virality.components.mesh.dynamic)

(v:define-component dynamic-mesh ()
  ((%geometry :reader geometry
              :initarg :geometry
              :initform nil)))

(defmethod v:on-component-initialize ((self dynamic-mesh))
  (with-slots (%geometry) self
    (unless %geometry
      (error "A dynamic mesh must have geometry defined."))
    (setf %geometry (geo::make-dynamic-geometry %geometry))))
