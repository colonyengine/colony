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
