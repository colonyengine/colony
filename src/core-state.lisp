(in-package :gear)

(defclass core-state ()
  ((%actor-initialize-db :accessor actor-initialize-db
                         :initarg :actor-initialize-db
                         :initform (make-hash-table))
   (%actor-active-db :accessor actor-active-db
                     :initarg :actor-active-db
                     :initform (make-hash-table))
   (%component-initialize-view :accessor component-initialize-view
                               :initarg :component-initialize-view
                               :initform (make-hash-table))
   (%component-active-view :accessor component-active-view
                           :initarg :component-active-view
                           :initform (make-hash-table))
   (%scene-tree :accessor scene-tree
                :initarg :scene-tree
                :initform NIL)))

(defun make-core-state (&rest initargs)
  (make-instance 'core-state initargs))


(defun add-initializing-actor (core-state actor
                               initializer-thunk-list)

  ;; Store initaizing actor
  (setf (gethash actor (game-object-initialize-db core-state)) actor)

  ;; Store all associated components for actor.
  (maphash (lambda (k v)
             (setf (gethash k (component-initialize-view core-state)) v))
           (components actor)))
