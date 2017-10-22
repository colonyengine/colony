(in-package :gear)

(defclass component ()
  ((%state :accessor state
           :initarg :state)
   (%actor :accessor actor
           :initarg :actor)))

(defgeneric make-component (component-type &rest initargs)
  (:method ((component-type symbol) &rest initargs)
    (apply #'make-instance component-type initargs)))

(defgeneric add-component (actor component)
  (:method ((actor actor) (component component))
    (setf (gethash component (components actor)) component)
    (push component (gethash (class-name (class-of component))
                             (components-by-type actor)))))

(defgeneric get-components (component-type actor)
  (:documentation "Return a list of the components with this COMPONENT-TYPE in
the ACTOR. Do not modify the structure of the returned list.")
  (:method ((component-type symbol) (actor actor))
    (gethash component-type (components-by-type actor))))

(defgeneric get-component (component-type actor)
  (:documentation "Return the _first found_ component of COMPONENT-TYPE in the
ACTOR. If there are multiple components of this type, it is unknown which one
will be returned first. Usually, there is only one component of any given
COMPONENT-TYPE in an ACTOR.")
  (:method ((component-type symbol) (actor actor))
    (first (get-components component-type actor))))

(defun add-multiple-components (actor components)
  (dolist (component components)
    (add-component actor component)))
