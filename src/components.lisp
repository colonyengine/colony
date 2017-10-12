(in-package :gear)

(defclass component ()
  ((%game-object :accessor game-object
                 :initarg :game-object)))

(defgeneric make-component (comp-type &rest args)
  (:documentation "This method should return an instance of the component
COMP-TYPE as initialied with the ARGS.")
  ;; And assume a default maker for all type name that are symbols.
  (:method ((comp-type symbol) &rest args)
    (apply #'make-instance comp-type args)))

(defgeneric add-component (gobj component)
  (:documentation "Add COMPONENT into the GAME-OBJECT."))

(defgeneric get-component (comp-type gobj)
  (:documentation "FIND the component of type COMP-TYPE in the GOBJ
  game-object."))

;; NOTE: Currently we don't check if we push two of the same type.
(defmethod add-component ((gobj game-object) (component component))
  (push component (components gobj)))

(defmethod get-component ((comp-type symbol) (gobj game-object))
  (find-if (lambda (inst)
             (typep inst comp-type))
           (components gobj)))
