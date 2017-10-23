(in-package :gear)

(defclass component ()
  ((%state :accessor state
           :initarg :state)
   (%actor :accessor actor
           :initarg :actor)))

(defmacro define-component (name super-classes &body slots)
  `(defclass ,(intern (symbol-name name) :gear) (component ,@super-classes)
     ,(loop :for (slot value) :in slots
            :collect `(,(intern (format nil "%~a" slot) :gear)
                       :accessor ,(intern (symbol-name slot) :gear)
                       :initarg ,(make-keyword slot)
                       :initform ,value))))

(defun component-type (component)
  (class-name (class-of component)))

(defgeneric make-component (component-type &rest initargs)
  (:method ((component-type symbol) &rest initargs)
    (apply #'make-instance component-type initargs)))

(defun add-component (actor component)
  (setf (gethash component (components actor)) component)
  (push component (gethash (component-type component)
                           (components-by-type actor))))

(defun get-components (component-type actor)
  "Return a list of the components with this COMPONENT-TYPE in the ACTOR. Do not
modify the structure of the returned list."
  (gethash component-type (components-by-type actor)))

(defun get-component (component-type actor)
  "Return the _first found_ component of COMPONENT-TYPE in the
ACTOR. If there are multiple components of this type, it is unknown which one
will be returned first. Usually, there is only one component of any given
COMPONENT-TYPE in an ACTOR."
  ;; TODO: account for when we actually do want to find a specific component of
  ;; the same type, probably by using FIND if an optional argument is suppliedp.
  (first (get-components component-type actor)))

(defun add-multiple-components (actor components)
  (dolist (component components)
    (add-component actor component)))
