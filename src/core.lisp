(in-package #:virality.engine)

(defvar *core-debug*)

(defclass core ()
  ((%resources :reader resources
               :initform (meta 'resources))
   (%options :accessor options)
   (%running-p :accessor running-p
               :initform t)
   (%rcache :reader rcache
            :initform (u:dict))
   (%clock :reader clock
           :initform nil)
   (%display :reader display
             :initform nil)
   (%scene-tree :reader scene-tree)
   (%cameras :accessor cameras
             :initform nil)
   (%input-data :reader input-data
                :initform (in::make-input-data))
   (%shaders :accessor shaders)
   (%materials :accessor materials
               :initform (mat::make-materials-table))
   (%textures :accessor textures
              :initform (%make-textures-table))
   (%context :reader context)
   (%tables :reader tables
            :initform (make-instance 'bookkeeping-tables))
   (%call-flows :reader call-flows
                :initform (u:dict))
   (%collider-system :accessor collider-system
                     :initform nil)
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (u:dict #'equalp))
   (%recompilation-queue :reader recompilation-queue
                         :initarg :recompilation-queue
                         :initform (queues:make-queue :simple-cqueue))))

(defclass bookkeeping-tables ()
  ((%component-search-table :reader component-search-table
                            :initform (u:dict))
   (%component-preinit-by-type-view :reader component-preinit-by-type-view
                                    :initform (u:dict))
   (%component-init-by-type-view :reader component-init-by-type-view
                                 :initform (u:dict))
   (%component-active-by-type-view :reader component-active-by-type-view
                                   :initform (u:dict))
   (%component-predestroy-view :reader component-predestroy-view
                               :initform (u:dict))
   (%component-destroy-by-type-view :reader component-destroy-by-type-view
                                    :initform (u:dict))
   (%components-by-id :reader components-by-id
                      :initform (u:dict #'equalp))
   (%actor-predestroy-view :reader actor-predestroy-view
                           :initform (u:dict))
   (%actor-preinit-db :reader actor-preinit-db
                      :initform (u:dict))
   (%actor-init-db :reader actor-init-db
                   :initform (u:dict))
   (%actor-active-db :reader actor-active-db
                     :initform (u:dict))
   (%actor-destroy-db :reader actor-destroy-db
                      :initform (u:dict))
   (%actors-by-id :reader actors-by-id
                  :initform (u:dict #'equalp))
   (%objects-by-uuid :reader objects-by-uuid
                     :initform (u:dict #'equalp))))

(defun pending-preinit-tasks-p (core)
  "Return T if there are ANY components or actors in the preinit data structures
in CORE."
  (or (plusp (hash-table-count (actor-preinit-db (tables core))))
      (block done
        (u:do-hash-values (v (component-preinit-by-type-view (tables core)))
          (when (plusp (hash-table-count v))
            (return-from done t))))))

(defun pending-predestroy-tasks-p (core)
  "Return T if there are ANY components or actors that are in the predestroy
  data structures in CORE."
  (or (plusp (hash-table-count (component-predestroy-view (tables core))))
      (plusp (hash-table-count (actor-predestroy-view (tables core))))))

(defun pending-destroy-tasks-p (core)
  "Return T of there are ANY components or actors that are in the destroy data
structures in CORE."
  (or (plusp (hash-table-count (actor-destroy-db (tables core))))
      (block done
        (u:do-hash-values (v (component-destroy-by-type-view (tables core)))
          (when (plusp (hash-table-count v))
            (return-from done t))))))

(defun make-scene-tree (core)
  (let* ((context (context core))
         (actor (make-actor context :id 'universe :display-id "Universe"))
         (transform (make-component context 'comp:transform :actor actor)))
    (attach-component actor transform)
    (spawn-actor actor :parent nil)
    (execute-flow core :default
                  'initialize-phase
                  'entry/initialize-phase
                  :come-from-state-name 'ef-make-scene-tree)
    (setf (slot-value core '%scene-tree) actor)))

(defgeneric shared-storage (context key)
  (:method (context key)
    (u:href (shared-storage-table context) key))
  (:method (context (key component))
    (shared-storage context (component-type key))))

(defgeneric (setf shared-storage) (value context key)
  (:method (value context key)
    (setf (u:href (shared-storage-table context) key) value))
  (:method (value context (key component))
    (setf (shared-storage context (component-type key)) value)))

;; This might call rcache-construct if needed.
(defmethod rcache-lookup (context (entry-type symbol) &rest keys)
  (with-slots (%rcache) (core context)
    (ensure-nested-hash-table %rcache
                              ;; NOTE: 'eq is for the rcache table itself.
                              (list* 'eq (rcache-layout entry-type))
                              (list* entry-type keys))
    (multiple-value-bind (value presentp)
        (apply #'u:href %rcache (list* entry-type keys))
      (unless presentp
        (setf value (apply #'rcache-construct context entry-type keys)
              (apply #'u:href %rcache (list* entry-type keys)) value))
      value)))

;; This might call rcache-dispose if needed.
(defmethod rcache-remove (context (entry-type symbol) &rest keys)
  (with-slots (%rcache) (core context)
    (multiple-value-bind (value presentp)
        (apply #'u:href %rcache (list* entry-type keys))
      (when presentp
        (remhash (apply #'u:href %rcache (list* entry-type keys)) %rcache)
        (rcache-dispose context entry-type value)))))
