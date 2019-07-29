(in-package #:first-light.components)

(define-component mesh ()
  ((%location :reader location
              :initarg :location
              :initform nil)
   (%index :reader index
           :initarg :index
           :initform 0)
   (%primitives :reader primitives))
  ((:cached-mesh-data equalp eql)))

(defun draw-mesh (mesh &optional count)
  (dolist (primitive (primitives mesh))
    (funcall (%fl:draw-func primitive) :instance-count count)))

(defmethod on-component-initialize ((self mesh))
  (with-slots (%primitives) self
    (let ((location (a:ensure-list (location self)))
          (index (index self)))
      (unless location
        (error "A mesh component must have a location set."))
      (with-shared-storage
          (context (context self))
          ((cached-mesh mesh-present-p
                        ('mesh :cached-mesh-data location index)
                        (%fl:load-gltf
                         (apply #'find-resource context location) index)))
        (setf %primitives cached-mesh)))))
