(in-package :gear)

(defclass component ()
  ((%state :accessor state
           :initarg :state
           :initform :initialize)
   (%actor :accessor actor
           :initarg :actor)
   (%initializer-thunk :accessor initializer-thunk
                       :initarg :initializer-thunk
                       :initform NIL)))

(defmacro define-component (name super-classes &body slots)
  `(defclass ,name (component ,@super-classes)
     ,(loop :for slot :in slots
            :collect
            (destructuring-bind (slot-name slot-value &key type) slot
              (append
               `(,(symbolicate '% slot-name)
                 :accessor ,slot-name
                 :initarg ,(make-keyword slot-name)
                 :initform ,slot-value)
               (when type
                 `(:type ,type)))))))

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

  (first (get-components component-type actor)))

(defun add-multiple-components (actor components)
  (dolist (component components)
    (add-component actor component)))

(defgeneric initialize-component (component context)
  (:method ((component component) (context context))))

(defgeneric update-component (component context)
  (:method ((component component) (context context))))

(defgeneric render-component (component context)
  (:method ((component component) (context context))))

(defgeneric destroy-component (component context)
  (:method ((component component) (context context))))
