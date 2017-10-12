(in-package :gear)

(defclass game-object ()
  ((%id :accessor id
        :initarg :id)
   (%components :accessor components
                :initarg :components
                :initform nil)))

(defmethod print-object ((object game-object) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (id object))))
