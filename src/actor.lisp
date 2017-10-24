(in-package :gear)

(defclass actor ()
  ((%id :accessor id
        :initarg :id)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%components :accessor components
                :initarg :components
                :initform (make-hash-table))
   (%components-by-type :accessor components-by-type
                        :initarg :components-by-type
                        :initform (make-hash-table))))

(defmethod print-object ((object actor) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (id object))))
