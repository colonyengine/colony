(in-package :gear)

(defclass game-object ()
  ((%components :accessor components
                :initarg :components
                :initform nil)))
