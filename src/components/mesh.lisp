(in-package #:colony.component)

(c:define-component mesh ()
  ((%asset :reader asset
           :initarg :asset
           :initform nil)
   (%name :reader name
          :initarg :name
          :initform nil)
   (%index :reader index
           :initarg :index
           :initform 0)
   (%primitive :reader primitive)))

(defmethod c:on-component-initialize ((self mesh))
  (with-slots (%asset %name %index %primitive) self
    (unless %asset
      (error "A mesh component must have an asset."))
    (unless %name
      (error "A mesh component must have a name."))
    (let* ((context (c:context self))
           (path (c::resolve-path %asset))
           (gltf (c:with-asset-cache context :mesh path
                   (c::load-gltf path)))
           (mesh (u:href (c::meshes gltf) %name)))
      (unless mesh
        (error "Mesh name ~s not found in mesh file ~s." %name path))
      (setf %primitive (aref (c::primitives mesh) %index)))))

(defmethod c:on-component-slave-render ((master render) (self mesh))
  (let ((instance-count (c::instances (comp:material master))))
    (funcall (c::draw-func (primitive self)) instance-count)))
