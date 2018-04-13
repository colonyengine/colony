(in-package :fl.comp.mesh)

(define-component mesh ()
  (location :default nil :shared t)
  (id :default 0 :shared t)
  (primitives :default nil))

(defun load-mesh (context location id)
  (let ((core-state (core-state context)))
    (fl.assets:load-mesh (find-resource core-state location) id)))

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
