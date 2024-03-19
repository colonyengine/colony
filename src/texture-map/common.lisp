(in-package #:colony.texture-map)

(defun make-texture-map (texture-map-type &rest init-args)
  (apply #'make-instance texture-map-type init-args))

(defun make-data-store (data-store-type num-elements &rest init-args)
  "A factory function that will produce the DATA-STORE-TYPE as tasked that
can contain NUM-ELEMENTS of elements. INIT-ARGS are the usual plist of
slot initargs and values."
  (apply #'make-instance data-store-type
         :data-elements (make-array num-elements)
         init-args))

(defun make-cube-face (&rest init-args)
  (apply #'make-instance 'cube-face init-args))

(defun make-data-element (&rest init-args)
  (apply #'make-instance 'data-element init-args))

(defun make-texture-map-descriptor (name ast extra-asts user-form)
  (make-instance 'texture-map-descriptor
                 :name name :ast ast :extra-asts extra-asts
                 :user-form user-form))
