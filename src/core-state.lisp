(in-package :first-light)

(defclass core-state ()
  ((%actor-initialize-db :reader actor-initialize-db
                         :initform (make-hash-table))
   (%component-initialize-by-type-view :reader component-initialize-by-type-view
                                       :initform (make-hash-table))
   (%actor-active-db :reader actor-active-db
                     :initform (make-hash-table))
   (%component-active-view :reader component-active-view
                           :initform (make-hash-table))
   (%user-package :reader user-package
                  :initarg :user-package)
   (%display :reader display)
   (%scene-tree :reader scene-tree)
   (%cameras :accessor cameras
             :initform nil)
   (%shaders :accessor shaders)
   (%vertex-metadata :accessor vertex-metadata)
   (%context :reader context)
   (%call-flows :reader call-flows
                :initform (make-hash-table))
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (make-hash-table :test #'equalp))
   (%scenes :reader scenes
            :initform (make-hash-table))))

(defun %make-scene-tree (core-state)
  (let* ((actor (make-actor :id (make-gensym '@universe) :scene t))
         (transform (make-component 'transform :actor actor)))
    (add-component actor transform)
    (realize-actor core-state actor)
    (realize-component core-state transform)
    actor))

(defun make-core-state (&rest args)
  (let ((core-state (apply #'make-instance 'core-state args)))
    (with-slots (%context %scene-tree) core-state
      (setf %context (make-instance 'context :core-state core-state)
            %scene-tree (%make-scene-tree core-state)))
    core-state))

(defun realize-phase-commit (core-state)
  "This function removes all elements from the
component-initialize-thunks-db slot and the actor-initialize-db in the
CORE-STATE. It is assumed they have been processed appropriately."
  ;; remove all the actors from initialization phase.
  (clrhash (actor-initialize-db core-state))
  ;; remove the components from the typed hashes of the component initialization
  ;; view.
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (clrhash v))
   (component-initialize-by-type-view core-state)))

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
