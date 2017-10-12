(in-package :gear)

(defclass component ()
  ((%game-object :accessor game-object
                :initarg :game-object)))
