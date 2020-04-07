(in-package #:virality.engine)

(defclass core ()
  ((%project :reader project
             :initarg :project)
   (%assets :reader assets
            :initform (meta 'assets))
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
              :initform (tex::make-textures-table))
   (%context :reader context)
   (%tables :reader tables
            :initform (make-instance 'bookkeeping-tables))
   (%call-flows :reader call-flows
                :initform (u:dict))
   (%collider-system :accessor collider-system
                     :initform nil)
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (u:dict #'equalp))
   (%thread-pool :reader thread-pool)))

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
   (%kernels-by-uuid :reader kernels-by-uuid
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
         (transform (make-component context 'c/xform:transform :actor actor)))
    (attach-component actor transform)
    (spawn-actor actor :parent nil)
    (execute-flow core :default
                  'initialize-phase
                  'entry/initialize-phase
                  :come-from-state-name 'ef-make-scene-tree)
    (setf (slot-value core '%scene-tree) actor)))
