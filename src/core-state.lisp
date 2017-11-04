(in-package :gear)

(defvar *scene-table*)
(defvar *context*)

(defclass core-state ()
  ((%actor-initialize-db :accessor actor-initialize-db
                         :initarg :actor-initialize-db
                         :initform (make-hash-table :test #'eq))
   (%component-initialize-by-type-view
    :accessor component-initialize-by-type-view
    :initarg :component-initialize-by-type-view
    :initform (make-hash-table :test #'eq))
   (%actor-active-db :accessor actor-active-db
                     :initarg :actor-active-db
                     :initform (make-hash-table :test #'eq))
   (%component-active-view :accessor component-active-view
                           :initarg :component-active-view
                           :initform (make-hash-table :test #'eq))
   (%scene-table :accessor scene-table
                 :initarg :scene-table
                 :initform (make-hash-table :test #'eq))
   (%scene-tree :accessor scene-tree
                :initarg :scene-tree
                :initform nil)
   (%call-flow-table :accessor call-flow-table
                     :initarg :call-flow-table
                     :initform (make-hash-table :test #'eq))
   (%display :accessor display
             :initarg :display
             :initform nil)
   (%context :accessor context
             :initarg :context
             :initform (make-hash-table :test #'eq))))

(defun make-core-state (&rest initargs)
  (apply #'make-instance 'core-state initargs))

(defun add-scene-tree-root (core-state actor)
  (setf (scene-tree core-state) actor))

(defun merge-context (core-state context)
  (maphash
   (lambda (k v)
     (setf (gethash k (context core-state)) v))
   context)
  core-state)

(defun merge-scene-table (core-state scene-table)
  (maphash
   (lambda (k v)
     (setf (gethash k (scene-table core-state)) v))
   scene-table)
  core-state)

(defun merge-call-flow-table (core-state call-flow-table)
  (maphash
   (lambda (k v)
     (setf (gethash k (call-flow-table core-state)) v))
   call-flow-table)
  core-state)

(defun spawn-actor (core-state actor)
  "Take the ACTOR and INITIALIZER-THUNK-LIST and place into the initializing
db's and view's in the CORE-STATE. The actor is not yet in the scene
and the main loop protocol will not be called on it or its components."

  ;; store actor in conceptual storage location.
  (setf (gethash actor (actor-initialize-db core-state)) actor)

  ;; put all components into the hash which represents the fact we need to
  ;; complete their initialization by type.
  (maphash
   (lambda (k v)
     (let ((component-type-name (component-type v)))
       (multiple-value-bind (comp-type-ht presentp)
           (gethash component-type-name
                    (component-initialize-by-type-view core-state))
         (unless presentp
           (let ((ht (make-hash-table :test #'eq)))
             (setf (gethash component-type-name
                            (component-initialize-by-type-view core-state))
                   ht)
             (setf comp-type-ht ht)))
         (setf (gethash k comp-type-ht) v))))
   (components actor)))

(defun realize-components (core-state component-ht)
  "For all component values in the COMPONENT-HT hash table, run their
initialize-thunks, set them :active, and put them into the active component
view."
  (maphash
   (lambda (k component)
     (declare (ignore k))
     (when-let ((thunk (initializer-thunk component)))
       (funcall thunk)
       (setf (initializer-thunk component) nil))
     (setf (state component) :active
           (gethash component (component-active-view core-state)) component))
   component-ht))

(defun realize-actor (core-state actor)
  "Change the ACTOR's state to :active, then place into the actor-active-db
in the CORE-STATE."
  (setf (state actor) :active
        (gethash actor (actor-active-db core-state)) actor))

(defun realize-phase-commit (core-state)
  "This function removes all elements from the
component-initialize-thunks-db slot and the actor-initialize-db in the
CORE-STATE. It is assumed they have been processed appropriately."
  ;; remove all the actors from initialization phase.
  (clrhash (actor-initialize-db core-state))

  ;; and remove the components from the typed hashes of the component
  ;; initialization view.
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (clrhash v))
   (component-initialize-by-type-view core-state)))
