(in-package #:first-light.components)

(define-component dynamic-mesh ()
  ((%geometry :reader geometry
              :initarg :geometry
              :initform nil)))

(defmethod on-component-initialize ((self dynamic-mesh))
  (with-slots (%geometry) self
    (unless %geometry
      (error "A dynamic mesh must have geometry defined."))
    (setf %geometry (make-dynamic-geometry %geometry))))
