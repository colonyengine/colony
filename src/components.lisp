(in-package :gear)

(defclass component ()
  ((%state :accessor state
           :initarg :state)
   (%game-object :accessor game-object
                 :initarg :game-object)))

(defgeneric make-component (component-type &rest args)
  (:method ((component-type symbol) &rest args)
    (apply #'make-instance component-type args)))

(defgeneric add-component (game-object component)
  (:method ((game-object game-object) (component component))
    (setf (gethash component (components game-object)) component)
    (push component (gethash (class-name (class-of component))
                             (components-by-type game-object)))))

(defgeneric get-component (component-type game-object)
  (:documentation "Return the _first found_ component of COMPONENT-TYPE in
the GAME-OBJECT.  If there are multiple components of this type, it is
unknown which one will be returned first. Usually, there is only one
component of any given COMPONENT-TYPE in a GAME-OBJECT.")
  (:method ((component-type symbol) (game-object game-object))
    (car (gethash component-type
                  (components-by-type game-object)))))

(defgeneric get-components (component-type game-object)
  (:documentation "Return a list of the components with this COMPONENT-TYPE in
the GAME-OBJECT. Do not modify the structure of the returned list.")
  (:method ((component-type symbol) (game-object game-object))
    (gethash component-type (components-by-type game-object))))

(defun add-multiple-components (game-object components)
  (dolist (component components)
    (add-component game-object component)))
