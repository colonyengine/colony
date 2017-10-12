(in-package :gear)

(defclass game-object ()
  ((%id :accessor id
        :initarg :id)
   (%components :accessor components
                :initarg :components
                :initform nil)))
