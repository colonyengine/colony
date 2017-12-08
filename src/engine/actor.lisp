(in-package :fl.core)

(defclass actor ()
  ((%id :reader id
        :initarg :id)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%components :reader components
                :initform (make-hash-table))
   (%components-by-type :reader components-by-type
                        :initform (make-hash-table))
   (%scene :accessor scene
           :initarg :scene)
   (%core-state :reader core-state
                :initarg :core-state)))

(defmethod print-object ((object actor) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (id object))))

(defun make-actor (context &rest args)
  (apply #'make-instance 'actor :core-state (core-state context) args))

(defun add-component (actor component)
  (setf (gethash component (components actor)) component)
  (push component (gethash (component-type component)
                           (components-by-type actor))))

(defun add-multiple-components (actor components)
  (dolist (component components)
    (add-component actor component)))

(defun actor-components-by-type (actor component-type)
  "Get a list of all components of type COMPONENT-TYPE for the given ACTOR."
  (gethash component-type (components-by-type actor)))

(defun actor-component-by-type (actor component-type)
  "Get the first component of type COMPONENT-TYPE for the given ACTOR.
Returns T as a secondary value if there exists more than one component of that
type."
  (let* ((qualified-type (qualify-component (core-state actor) component-type))
         (components (actor-components-by-type actor qualified-type)))
    (values (first components)
            (> (length components) 1))))

(defun spawn-actor (core-state actor)
  "Take the ACTOR and INITIALIZER-THUNK-LIST and place into the initializing
db's and view's in the CORE-STATE. The actor is not yet in the scene and the
main loop protocol will not be called on it or its components."
  (setf (gethash actor (actor-initialize-db core-state)) actor)
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (setf (type-table
            (canonicalize-component-type (component-type v) core-state)
            (component-initialize-by-type-view core-state))
           v))
   (components actor)))

(defun realize-actor (core-state actor)
  "Change the ACTOR's state to :active, then place into the actor-active-db in
the CORE-STATE."
  (setf (state actor) :active
        (gethash actor (actor-active-db core-state)) actor))
