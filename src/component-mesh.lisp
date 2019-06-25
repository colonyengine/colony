(in-package #:first-light.components)

(define-component mesh ()
  ((location :default nil)
   (index :default 0)
   (primitives :default nil))
  ((:cached-mesh-data equalp eql)))

(defun draw-mesh (mesh &optional count)
  (dolist (primitive (primitives mesh))
    (funcall (%fl:draw-func primitive) :instance-count count)))

(defmethod on-component-initialize ((self mesh))
  (with-accessors ((context context) (location location) (index index)
                   (primitives primitives))
      self
    (unless location
      (error "A mesh component must have a location set."))
    (let ((location (a:ensure-list location)))
      (with-shared-storage
          (context context)
          ((cached-mesh mesh-present-p
                        ('mesh :cached-mesh-data location index)
                        (%fl:load-gltf
                         (apply #'find-resource context location) index)))
        (setf primitives cached-mesh)))))
