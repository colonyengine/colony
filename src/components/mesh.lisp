(in-package :fl.comp.mesh)

(define-component mesh ()
  ;; Component slot descriptions all in one form, like defclass
  ((location :default nil :shared t)
   (id :default 0 :shared t)
   (primitives :default nil))

  ;; Shared storage namespace definitions. First form is a namespace symbol
  ;; which could be in a different package if desired, the &rest is a
  ;; sequence of hash table test functions that will be consulted when
  ;; making nested hash tables.
  (;; Have we loaded this mesh before.
   ;; Key Path: location[a case sensitive string], id[an integer]
   ;; Value: loaded mesh data
   (:cached-mesh-data #'equal #'eql)))


(defun %load-mesh (context location id)
  (load-mesh (find-resource context location) id))

(defmethod fl.comp.mesh-renderer:draw-mesh ((mesh mesh) &key (instance-count 1))
  (dolist (primitive (primitives mesh))
    (funcall (draw-func primitive) :instance-count instance-count)))

(defmethod initialize-component ((component mesh) (context context))
  (with-accessors ((location location) (id id) (primitives primitives)) component
    (unless location
      (error "A mesh component must have a location set."))

    ;; TODO: the below code will get tested and committed when the new
    ;; shared storage system is working.

    ;; Long Form: The fully flexible way to use shared storage.
    #++(multiple-value-bind (cached-mesh presentp)
           (ss-href context 'mesh :cached-mesh-data location id)
         ;; create if not present.
         (unless presentp
           ;; Here we choose to use the same lookup key as before, but it is not
           ;; true that this will always be the case--just most of the time.
           (setf cached-mesh
                 (%load-mesh context location id)

                 (ss-href context 'mesh :cached-mesh-data location id)
                 cached-mesh))

         ;; cached-mesh is now bound with previously found or new cache entry.
         (setf primitives (primitives cached-mesh)))


    ;; Short Form: Make some assumptions and it is short.
    ;;
    ;; Assumes you store the generated value in the same place
    ;; you tried to find it.
    #++(with-new-shared-storage context
         ;; Supports multiple binding forms, if desired.
         (;; First: The lexical var which will hold the result, either looked
          ;; up or newly created.
          ;;
          ;; Second: The component-name and namespace to lookup, and then a
          ;; &rest of key forms in order.
          (cached-mesh ('mesh :cached-mesh-data location id)
                       ;; Third: when not present, run this form and store
                       ;; results in setf lookup form, then assign
                       ;; the lexical var this value.
                       (%load-mesh context location id)))

         ;; Body of the with-new-shared-storage
         (setf primitives (primitives cached-mesh)))



    ;; TODO: current shitty bad old method, going to be deleted.
    (with-shared-storage (mesh
                          store
                          cached-mesh
                          (values location id)
                          (values location id (%load-mesh context location id)))
      (setf primitives (primitives cached-mesh)))))
