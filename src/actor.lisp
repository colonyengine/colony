(in-package :gear)

(defclass actor ()
  ((%id :reader id
        :initarg :id)
   (%state :accessor state
           :initform :initialize)
   (%components :reader components
                :initform (make-hash-table :test #'eq))
   (%components-by-type :reader components-by-type
                        :initform (make-hash-table :test #'eq))
   (%scene :accessor scene
           :initarg :scene)))

(defmethod print-object ((object actor) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (id object))))
