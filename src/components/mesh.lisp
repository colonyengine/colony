(in-package :fl.comp.mesh)

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
   ;; Key Path: location[a case sensitive string], id[an integer]
   ;; Value: loaded mesh data
   ;; NOTE the tests must only be EQ EQL EQUAL and EQUALP and must be the
   ;; raw symbols, no quotes or #' or (function eq) allowed.
   (:cached-mesh-data equal eql)))

(defun %load-mesh (context location id)
  (load-mesh (find-resource context location) id))

(defmethod fl.comp.mesh-renderer:draw-mesh ((mesh mesh) &key (instance-count 1))
  (dolist (primitive (primitives mesh))
    (funcall (draw-func primitive) :instance-count instance-count)))

(defmethod initialize-component ((component mesh) (context context))
  (with-accessors ((location location) (id id) (primitives primitives)) component
    (unless location
      (error "A mesh component must have a location set."))

    ;; TODO: Before removing this, document how shared storage work in the wiki.
    ;; Describe both long and short forms, plus the assumptions they make.

    ;; Long Form: The fully flexible way to use shared storage.
    #++(multiple-value-bind (cached-mesh presentp)
           (ss-href context 'mesh :cached-mesh-data location id)
         (unless presentp
           ;; Here we choose to use the same lookup key as before, but it is not
           ;; true that this will always be the case--just most of the time.
           (setf cached-mesh
                 (%load-mesh context location id)

                 (ss-href context 'mesh :cached-mesh-data location id)
                 cached-mesh))

         ;; cached-mesh is now bound with previously found or new cache entry.
         (setf primitives cached-mesh))


    ;; Short Form: Make some assumptions and it is short.
    ;;
    ;; Assumes you store the generated value in the same place
    ;; you tried to find it.
    (with-shared-storage (context context)
                         ((cached-mesh mesh-presentp
                                       ;; this is what type to look it up in,
                                       ;; and the key.
                                       ('mesh :cached-mesh-data location id)
                                       ;; Store this value if not in cache.
                                       (%load-mesh context location id)))

      ;; Body of the with-shared-storage
      (setf primitives cached-mesh))))
