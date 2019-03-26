(in-package :first-light.components)

(define-component mesh ()
  ((location :default nil)
   (id :default 0)
   (primitives :default nil))
  ((:cached-mesh-data equalp eql)))

(defun draw-mesh (mesh &optional count)
  (dolist (primitive (primitives mesh))
    (funcall (fl.geom:draw-func primitive) :instance-count count)))

(defmethod on-component-initialize ((self mesh))
  (with-accessors ((context context) (location location) (id id)
                   (primitives primitives))
      self
    (unless location
      (error "A mesh component must have a location set."))
    (let ((location (au:ensure-list location)))
      (with-shared-storage
          (context context)
          ((cached-mesh mesh-present-p
                        ('mesh :cached-mesh-data location id)
                        (fl.geom:load-gltf
                         (apply #'find-resource context location) id)))
        (setf primitives cached-mesh)))))
