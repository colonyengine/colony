(in-package #:first-light.components)

(define-component dynamic-mesh ()
  ((geometry :default nil)))

(defmethod on-component-initialize ((self dynamic-mesh))
  (with-accessors ((geometry geometry)) self
    (unless geometry
      (error "A dynamic mesh must have geometry defined."))
    (setf geometry (funcall (u:href (meta '%fl::dynamic-geometry) geometry)))))
