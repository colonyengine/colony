(in-package :fl.comp.mesh)

(define-component mesh ()
  (location :default nil :shared t)
  (id :default 0 :shared t)
  (primitives :default nil))

(defun load-mesh (context location id)
  (fl.assets:load-mesh (find-resource (core-state context) location) id))

(defmethod fl.comp.mesh-renderer:draw-mesh ((mesh mesh) &key (instance-count 1))
  (dolist (primitive (primitives mesh))
    (funcall (fl.assets:draw-func primitive) :instance-count instance-count)))

(defmethod initialize-component ((component mesh) (context context))
  (with-accessors ((location location) (id id) (primitives primitives)) component
    (unless location
      (error "A mesh component must have a location set."))
    (with-shared-storage (mesh
                          store
                          cached-entry
                          (values location id)
                          (values location id (load-mesh context location id)))
      (setf primitives (primitives cached-entry)))))
