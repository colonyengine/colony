(in-package #:virality.texture-map)

(defun make-texture-map (texture-map-type &rest init-args)
  (apply #'make-instance texture-map-type init-args))

(defun make-data-store (data-store-type num-elements &rest init-args)
  (apply #'make-instance data-store-type
         :data-elements (make-array num-elements)
         init-args))

(defun make-cube-face (&rest init-args)
  (apply #'make-instance 'cube-face init-args))

(defun make-data-element (&rest init-args)
  (apply #'make-instance 'data-element init-args))
