(in-package :gear)

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
   (%camera :accessor camera
            :initform nil)
   (%scene-tree :accessor scene-tree)
   (%shaders :accessor shaders
             :initform nil)
   (%context :reader context
             :initform (make-instance 'context))
   (%call-flows :reader call-flows
                :initform (make-hash-table))
   (%scenes :reader scenes
            :initform (make-hash-table))))

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
           (let ((ht (make-hash-table)))
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
  (map-nodes
   (lambda (x)
     ;; TODO: Instead of checking each camera component type, we should have a
     ;; means to identify any camera more flexibly.
     (when-let ((camera (or (get-component 'camera (actor x))
                            (get-component 'tracking-camera (actor x)))))
       (when (activep camera)
         (return-from find-active-camera camera))))
   (get-component 'transform (scene-tree core-state))))
