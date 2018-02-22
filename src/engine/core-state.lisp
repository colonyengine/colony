(in-package :fl.core)

(defclass core-state ()
  ((%user-package :reader user-package
                  :initarg :user-package)
   (%display :reader display)
   (%scene-tree :reader scene-tree)
   (%cameras :accessor cameras
             :initform nil)
   (%shaders :accessor shaders)
   (%context :reader context)
   (%tables :reader tables
            :initform (make-instance 'bookkeeping-tables))
   (%call-flows :reader call-flows
                :initform (make-hash-table))
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (make-hash-table :test #'equalp))
   (%scenes :reader scenes
            :initform (make-hash-table))))

(defclass bookkeeping-tables ()
  ((%component-search-table :reader component-search-table
                            :initform (make-hash-table))
   (%component-preinit-by-type-view :reader component-preinit-by-type-view
                                    :initform (make-hash-table))
   (%component-init-by-type-view :reader component-init-by-type-view
                                 :initform (make-hash-table))
   (%component-active-by-type-view :reader component-active-by-type-view
                                   :initform (make-hash-table))
   (%component-predestroy-view :reader component-predestroy-view
                               :initform (make-hash-table))
   (%component-destroy-by-type-view :reader component-destroy-by-type-view
                                    :initform (make-hash-table))
   (%actor-predestroy-view :reader actor-predestroy-view
                           :initform (make-hash-table))
   (%actor-preinit-db :reader actor-preinit-db
                      :initform (make-hash-table))
   (%actor-init-db :reader actor-init-db
                   :initform (make-hash-table))
   (%actor-active-db :reader actor-active-db
                     :initform (make-hash-table))
   (%actor-destroy-db :reader actor-destroy-db
                      :initform (make-hash-table))))

(defun pending-preinit-tasks-p (core-state)
  "Return T if there are ANY components or actors in the preinit data structures in CORE-STATE."
  (or (plusp (hash-table-count (actor-preinit-db (tables core-state))))
      (block done
        (maphash
         (lambda (k v)
           (declare (ignore k))
           (when (plusp (hash-table-count v))
             (return-from done t)))
         (component-preinit-by-type-view (tables core-state))))))

(defun pending-predestroy-tasks-p (core-state)
  "Return T if there are ANY components or actors that are in the predestroy data structures in
CORE-STATE."
  (or (plusp (hash-table-count (component-predestroy-view (tables core-state))))
      (plusp (hash-table-count (actor-predestroy-view (tables core-state))))))

(defun pending-destroy-tasks-p (core-state)
  "Return T of there are ANY components or actors that are in the destroy data structures in
CORE-STATE."
  (or (plusp (hash-table-count (actor-destroy-db (tables core-state))))
      (block done
        (maphash
         (lambda (k v)
           (declare (ignore k))
           (when (plusp (hash-table-count v))
             (return-from done t)))
         (component-destroy-by-type-view (tables core-state))))))

(defun %make-scene-tree (core-state)
  (with-slots (%context) core-state
    (let* ((actor (make-actor %context :id (alexandria:make-gensym '@universe) :scene t))
           (transform (make-component 'transform %context :actor actor)))
      (attach-component actor transform)
      (spawn-actor actor %context :parent nil)
      (execute-flow core-state :default 'initialize-phase 'entry/initialize-phase
                    :come-from-state-name 'ef-make-scene-tree)
      actor)))

(defun make-core-state (&rest args)
  (let ((core-state (apply #'make-instance 'core-state args)))
    (setf (slot-value core-state '%context) (make-instance 'context :core-state core-state))
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
