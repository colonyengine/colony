(in-package :gear)

(defclass game-object ()
  ((%components :accessor components
                :initarg :components
                :initform nil)))

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
