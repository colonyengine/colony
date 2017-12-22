(in-package :fl.core)

(defclass core-state ()
  (;; Anything that has been spawned into the world first starts here, in
   ;; pre-initialization.
   (%actor-preinitialize-db :accessor actor-preinitialize-db
                            :initform (make-hash-table))
   (%component-preinitialize-by-type-view
    :accessor component-preinitialize-by-type-view
    :initform (make-hash-table))

   ;; Then, preinitialized objects are moved here and the initialize-component
   ;; protocol is run on them. If that method on the components spawn more
   ;; actors and/or make more components, then they go into pre-initialize.
   ;; This prevents adding entries to a hash we're iterating. We also can
   ;; directly control the execution of initialize-component for only new
   ;; things that need it.
   (%actor-initialize-db :accessor actor-initialize-db
                         :initform (make-hash-table))
   (%component-initialize-by-type-view
    :accessor component-initialize-by-type-view
    :initform (make-hash-table))

   ;; Then, things are moved into the active db where they sit most of the time.
   (%actor-active-db :accessor actor-active-db
                     :initform (make-hash-table))
   (%component-active-by-type-view :accessor component-active-by-type-view
                                   :initform (make-hash-table))

   ;; When we mark an actor or component for destruction, we place it here.
   ;; These doesn't have to be any kind of a type view.
   (%actor-predestroy-view :accessor actor-predestroy-view
                           :initform (make-hash-table))
   (%component-predestroy-view :accessor component-predestroy-view
                               :initform (make-hash-table))
   ;; When we're actually going to destroy the marked actors/componets, we
   ;; trace additional actors/components we need to destroy and everything ends
   ;; up here. Then we actually destroy them (which might cause other things
   ;; to be destroyed, so we repeat this cycle. This must be a type view
   ;; since we need to call the protocol appropriately.
   (%actor-destroy-db :accessor actor-destroy-db
                      :initform (make-hash-table))
   (%component-destroy-by-type-view :accessor component-destroy-by-type-view
                                    :initform (make-hash-table))



   (%user-package :reader user-package
                  :initarg :user-package)
   (%display :reader display)
   (%scene-tree :reader scene-tree)
   (%cameras :accessor cameras
             :initform nil)
   (%shaders :accessor shaders)
   (%vertex-metadata :accessor vertex-metadata)
   (%component-search-table :reader component-search-table
                            :initarg :component-search-table
                            :initform (make-hash-table))
   (%context :reader context)
   (%call-flows :reader call-flows
                :initform (make-hash-table))
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (make-hash-table :test #'equalp))
   (%scenes :reader scenes
            :initform (make-hash-table))))

(defun pending-preinit-tasks-p (core-state)
  (or
   ;; Any new actors?
   (> (hash-table-count
       (actor-preinitialize-db core-state)) 0)
   ;; Any new components of any type?
   (block done
     (maphash
      (lambda (k component-ht)
        (declare (ignore k))
        (when (> (hash-table-count component-ht) 0)
          (return-from done T)))
      (component-preinitialize-by-type-view core-state)))))

;; return T if there are either components or actors that have
;; reached their ttl in the predestroy tables.
(defun pending-predestroy-tasks-p (core-state)
  (or
   ;; Any components which ran out of time?
   (block done
     (maphash
      (lambda (k component)
        (declare (ignore k))
        (when (<= (ttl component) 0)
          (return-from done T)))
      (component-predestroy-view core-state)))

   ;; Any actors which ran out of time?
   (block done
     (maphash
      (lambda (actor-key actor-value)
        (declare (ignore actor-key))
        (when (<= (ttl actor-value) 0)
          (return-from done T)))
      (actor-predestroy-view core-state)))))

(defun %make-scene-tree (core-state)
  (let* ((actor (make-actor (context core-state)
                            :id (make-gensym '@universe)
                            :scene t))
         (transform (make-component 'transform
                                    (context core-state)
                                    :actor actor)))
    (add-component actor transform)
    (spawn-actor actor (context core-state) :parent nil)
    ;; Manually run the execute flow to get these actors and components into the
    ;; active state.
    (execute-flow core-state
                  :default 'initialize-phase 'ENTRY/INITIALIZE-PHASE
                  :come-from-state-name 'EF-MAKE-SCENE-TREE)
    actor))

(defun make-core-state (&rest args)
  (let ((core-state (apply #'make-instance 'core-state args)))
    (with-slots (%context) core-state
      (setf %context (make-instance 'context :core-state core-state)))
    core-state))

(defgeneric shared-storage (context key)
  (:method ((context context) key)
    (gethash key (shared-storage-table context)))
  (:method ((context context) (key component))
    (shared-storage context (component-type key))))

(defgeneric (setf shared-storage) (value context key)
  (:method (value (context context) key)
    (setf (gethash key (shared-storage-table context)) value))
  (:method (value (context context) (key component))
    (setf (shared-storage context (component-type key)) value)))

(defun find-resource (core-state path)
  (let ((core-path (get-path :first-light path))
        (user-path (get-path (user-package core-state) path)))
    (or (uiop:file-exists-p user-path)
        (uiop:file-exists-p core-path)
        (error "Resource not found: ~a" path))))
