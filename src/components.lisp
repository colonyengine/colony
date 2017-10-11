(in-package :gear)

(defclass component () ())

(defclass transform (component transformable)
  ((%parent :accessor parent
            :initarg :parent
            :initform nil)
   (%children :accessor children
              :initarg :children
              :initform nil)))

(defclass tags (component)
  ((%tags :accessor tags
          :initarg :tags
          :initform nil)))
