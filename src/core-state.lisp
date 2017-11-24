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
   (%display :reader display)
   (%scene-tree :reader scene-tree)
   (%cameras :accessor cameras
             :initform nil)
   (%shaders :accessor shaders
             :initform nil)
   (%context :reader context)
   (%call-flows :reader call-flows
                :initform (make-hash-table))
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (make-hash-table :test #'equalp))
   (%scenes :reader scenes
            :initform (make-hash-table))))

(defclass context ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%settings :reader settings
              :initform (make-hash-table))
   (%shaders :accessor shaders
             :initform nil)
   (%camera :accessor camera
            :initform nil)
   (%shared-storage-table :reader shared-storage-table
                          :initform (make-hash-table))))

(defun %make-scene-tree (core-state)
  (let* ((actor (make-actor :id (make-gensym '@universe) :scene t))
         (transform (make-component 'transform :actor actor)))
    (add-component actor transform)
    (realize-actor core-state actor)
    (realize-component core-state transform)
    actor))

(defun make-core-state ()
  (let ((core-state (make-instance 'core-state)))
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
  (:method :around ((context context) (key component))
    (call-next-method context (component-type key))))

(defgeneric (setf shared-storage) (value context key)
  (:method (value (context context) key)
    (setf (gethash key (shared-storage-table context)) value))
  (:method :around (value (context context) (key component))
    (call-next-method value context (component-type key))))
