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
