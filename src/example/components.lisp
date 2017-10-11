(in-package :gear/example)

(defclass hit-points (component)
  ((%hp :accessor hp
        :initarg :hp
        :initform 0)))
