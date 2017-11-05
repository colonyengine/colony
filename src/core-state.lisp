(in-package :gear)

(defclass core-state ()
  ((%actor-initialize-db :reader actor-initialize-db
                         :initform (make-hash-table :test #'eq))
   (%component-initialize-by-type-view :reader component-initialize-by-type-view
                                       :initform (make-hash-table :test #'eq))
   (%actor-active-db :reader actor-active-db
                     :initform (make-hash-table :test #'eq))
   (%component-active-view :reader component-active-view
                           :initform (make-hash-table :test #'eq))
   (%camera :accessor camera
            :initform nil)
   (%scene-table :reader scene-table
                 :initform (make-hash-table :test #'eq))
   (%scene-tree :accessor scene-tree)
   (%call-flow-table :reader call-flow-table
                     :initform (make-hash-table :test #'eq))
   (%display :reader display)
   (%context-table :reader context-table
                   :initform (make-hash-table :test #'eq))))

(defun add-scene-tree-root (core-state actor)
  (setf (scene-tree core-state) actor))

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

(defun find-active-camera (core-state)
  "Find the first active camera in CORE-STATE's scene tree and return the
component."
  (do-nodes
      (lambda (transform)
        (when-let ((camera-component (get-component 'camera (actor transform))))
          (when (and camera-component (activep camera-component))
            (return-from find-active-camera camera-component))))
    (get-component 'transform (scene-tree core-state))))
