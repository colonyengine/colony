(in-package :fl.comp.mesh)

(define-component mesh ()
  (location nil)
  (id 0)
  (primitives nil))

(defun load-mesh (context location id)
  (let ((core-state (core-state context)))
    (fl.assets:load-mesh (find-resource core-state location) id)))

(defmethod initialize-component ((component mesh) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (with-accessors ((location location) (id id) (primitives primitives))
        component
      (unless location
        (error "A mesh component must have a location set."))
      (unless store
        (setf store (make-instance 'mesh-shared-storage)))
      (multiple-value-bind (cached presentp) (cached-mesh store location id)
        (if presentp
            (progn
              (setf primitives (primitives cached))
              (simple-logger:emit :component.mesh.cache.used))
            (let* ((new-primitives (load-mesh context location id))
                   (cached (make-cached-mesh location id new-primitives)))
              (setf (cached-mesh store location id) cached
                    primitives (primitives cached))
              (simple-logger:emit :component.mesh.cache.created location)))))))
