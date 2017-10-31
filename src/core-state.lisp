(in-package :gear)

(defclass core-state ()
  ((%actor-initialize-db :accessor actor-initialize-db
                         :initarg :actor-initialize-db
                         :initform (make-hash-table :test #'eq))
   (%component-initialize-thunks-db :accessor component-initialize-thunks-db
                                    :initarg :component-initialize-thunks-db
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
                :initform NIL)
   (%call-flow-table :accessor call-flow-table
                     :initarg :call-flow-table
                     :initform (make-hash-table :test #'eq))
   (%context :accessor context ; to be defined later, hold delta time, etc.
             :initarg :context
             :initform nil)))


(defun make-core-state (&rest initargs)
  (apply #'make-instance 'core-state initargs))

(defun add-scene-tree-root (core-state actor)
  (setf (scene-tree core-state) actor))

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

(defun spawn-actor (core-state actor initializer-thunk-list)
  "Take the ACTOR and INITIALIZER-THUNK-LIST and place into the initializing
db's and view's in the CORE-STATE. The actor is not yet in the scene
and the main loop protocol will not be called on it or its components."
  (setf
   ;; Store initializing actor
   (gethash actor (actor-initialize-db core-state))
   actor)

  ;; store the component initializers by type name.
  (loop :for (comp-type thunk) :in initializer-thunk-list
        :do (push thunk (gethash comp-type
                                 (component-initialize-thunks-db core-state))))
  )

(defun realize-component (core-state component)
  "Run the initializer found in CORE-STATE for the COMPONENT, then
move it from the component-initialize-view in CORE-STATE to
component-active-view, then change its state to :active."

  nil)

(defun realize-actor (core-state actor)
  "Simply convert the ACTOR from :initialize to :active and move it
from CORE-STATE's actor-initialize-db to actor-active-db."
  nil)
