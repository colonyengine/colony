(in-package :gear)

(defclass component ()
  ((%game-object :accessor game-object
                 :initarg :game-object)))

(defgeneric add-component (gobj component)
  (:documentation
   "Add COMPONENT into the GAME-OBJECT."))

(defgeneric get-component (comp-type gobj)
  (:documentation
   "FIND the component of type COMP-TYPE in the GOBJ game-object."))

;; NOTE: Currently we don't check if we push two of the same type.
(defmethod add-component ((gobj game-object) (component component))
  (push component (components gobj)))

(defmethod get-component ((comp-type symbol) (gobj game-object))
  (find-if (lambda (inst)
             (typep inst comp-type))
           (components gobj)))
