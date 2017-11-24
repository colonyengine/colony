(in-package :first-light)

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
           :initarg :scene)))

(defmethod print-object ((object actor) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (id object))))

(defun make-actor (&rest args)
  (apply #'make-instance 'actor args))

(defun realize-actor (core-state actor)
  "Change the ACTOR's state to :active, then place into the actor-active-db
in the CORE-STATE."
  (setf (state actor) :active
        (gethash actor (actor-active-db core-state)) actor))

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
