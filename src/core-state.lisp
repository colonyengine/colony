(in-package :gear)

(defclass core-state ()
  ((%game-object-initialize-db :accessor game-object-initialize-db
                               :initarg :game-object-initialize-db
                               :initform (make-hash-table))
   (%game-object-active-db :accessor game-object-active-db
                           :initarg :game-object-active-db
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
