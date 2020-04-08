(in-package #:virality.components.mesh.static)

(v:define-component static-mesh ()
  ((%asset :reader asset
           :initarg :asset
           :initform nil)
   (%index :reader index
           :initarg :index
           :initform 0)
   (%data :reader data))
  ((:cached-mesh-data eq eql)))

(defmethod v:on-component-initialize ((self static-mesh))
  (with-slots (%asset %index %data) self
    (let ((context (v:context self)))
      (unless %asset
        (error "A mesh component must have an asset."))
      (let ((path (apply #'v::find-asset context %asset)))
        (v:with-shared-storage
            (context context)
            ((cached-mesh mesh-present-p
                          ('static-mesh :cached-mesh-data %asset %index)
                          (geo::load-static-geometry path %index)))
          (setf %data cached-mesh))))))
