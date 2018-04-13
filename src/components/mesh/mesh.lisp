(in-package :fl.comp.mesh)

(define-component mesh ()
  (location nil)
  (id 0)
  (primitives nil))

;; Read the docstring for this macro.
(define-shared-storage mesh (location id primitives) (location id))

(defun load-mesh (context location id)
  (let ((core-state (core-state context)))
    (fl.assets:load-mesh (find-resource core-state location) id)))

(defmethod initialize-component ((component mesh) (context context))
  (with-accessors ((location location) (id id) (primitives primitives))
      component
    (unless location
      (error "A mesh component must have a location set."))

    ;; Read the docstring for this macro.
    (with-shared-storage (mesh
                          (store (shared-storage context component))
                          cached-entry
                          (values location id)
                          (values location
                                  id
                                  (load-mesh context location id))
                          :fl.comp.mesh)

      ;; the body where 'store' and 'cached-entry' are lexically bound here.
      ;; cached-entry will always be valid here.
      (setf primitives (primitives cached-entry)))))
