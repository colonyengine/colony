(in-package #:virality.components)

(v:define-component mesh ()
  ((%asset :reader asset
           :initarg :asset
           :initform nil)
   (%name :reader name
          :initarg :name
          :initform nil)
   (%index :reader index
           :initarg :index
           :initform 0)
   (%primitive :reader primitive))
  ((:cached-mesh-data eq)))

(defmethod v:on-component-initialize ((self mesh))
  (with-slots (%asset %name %index %primitive) self
    (unless %asset
      (error "A mesh component must have an asset."))
    (unless %name
      (error "A mesh component must have a name."))
    (let* ((context (v:context self))
           (path (apply #'v::find-asset context %asset))
           (gltf (v:with-storage
                     (context context)
                     ((cached-mesh mesh-present-p
                                   ('mesh :cached-mesh-data %asset)
                                   (v::load-gltf path)))
                   cached-mesh))
           (mesh (u:href (v::meshes gltf) %name)))
      (unless mesh
        (error "Mesh name ~s not found in mesh file ~s." %name path))
      (setf %primitive (aref (v::primitives mesh) %index)))))
