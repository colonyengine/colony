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
   (%graphs :reader graphs
	    :initform (make-hash-table))
   (%scenes :reader scenes
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
     (symbol-macrolet
         ((component-type-name
            (gethash (component-type v)
                     (component-initialize-by-type-view core-state))))
       (multiple-value-bind (component-type-table presentp) component-type-name
         (unless presentp
           (let ((table (make-hash-table)))
             (setf component-type-name table
                   component-type-table table)))
         (setf (gethash k component-type-table) v))))
   (components actor)))

(defun realize-component (core-state component)
  (when-let ((thunk (initializer-thunk component)))
    (funcall thunk)
    (setf (initializer-thunk component) nil))
  (setf (state component) :active
        (gethash component (component-active-view core-state)) component))

(defun realize-components (core-state component-table)
  "For all component values in the COMPONENT-HT hash table, run their
initialize-thunks, set them :active, and put them into the active component
view."
  (maphash
   (lambda (k component)
     (declare (ignore k))
     (realize-component core-state component))
   component-table))

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
