(in-package :gear)

(defclass tags (component)
  ((%tags :accessor tags
          :initarg :tags
          :initform nil)))
