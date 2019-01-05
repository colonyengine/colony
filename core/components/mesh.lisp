(in-package :first-light.components)

(define-component mesh ()
  ;; Component slot descriptions all in one form, like defclass
  ((location :default nil)
   (id :default 0)
   (primitives :default nil))

  ;; Shared storage namespace definitions. First form is a namespace symbol
  ;; which could be in a different package if desired, the &rest is a
  ;; sequence of hash table test functions that will be consulted when
  ;; making nested hash tables.
  (;; Have we loaded this mesh before.
   ;; Key Path: location[a cons], id[an integer]
   ;; Value: loaded mesh data
   ;; NOTE the tests must only be EQ EQL EQUAL and EQUALP and must be the
   ;; raw symbols, no quotes or #' or (function eq) allowed.
   (:cached-mesh-data equalp eql)))

(defmethod draw-mesh ((mesh mesh) &key (instance-count 1))
  (dolist (primitive (primitives mesh))
    (funcall (fl.geom:draw-func primitive) :instance-count instance-count)))

(defmethod on-component-initialize ((self mesh))
  (with-accessors ((context context) (location location) (id id) (primitives primitives)) self
    (unless location
      (error "A mesh component must have a location set."))
    (let ((location (fl.util:ensure-list location)))
      (with-shared-storage
          (context context)
          ((cached-mesh mesh-present-p
                        ('mesh :cached-mesh-data location id)
                        (fl.geom:load-gltf (apply #'find-resource context location) id)))
        (setf primitives cached-mesh)))))
