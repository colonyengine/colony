(in-package :first-light)

(defclass component ()
  ((%state :accessor state
           :initarg :state
           :initform :initialize)
   (%actor :accessor actor
           :initarg :actor)
   (%initializer-thunk :accessor initializer-thunk
                       :initarg :initializer-thunk
                       :initform nil)))

(defmacro define-component (name super-classes &body slots)
  `(defclass ,name
       (,@(append (unless super-classes '(component))
                  super-classes))
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

(defun actor-components-by-type (actor component-type)
  "Get a list of all components of type COMPONENT-TYPE for the given ACTOR."
  (gethash component-type (components-by-type actor)))

(defun actor-component-by-type (actor component-type)
  "Get the first component of type COMPONENT-TYPE for the given ACTOR.
Returns T as a secondary value if there exists more than one component of that
type."
  (let ((components (actor-components-by-type actor component-type)))
    (values (first components)
            (> (length components) 1))))

(defun add-multiple-components (actor components)
  (dolist (component components)
    (add-component actor component)))

(defgeneric initialize-component (component context)
  (:method ((component component) (context context))))

(defgeneric physics-update-component (component context)
  (:method ((component component) (context context))))

(defgeneric update-component (component context)
  (:method ((component component) (context context))))

(defgeneric render-component (component context)
  (:method ((component component) (context context))))

(defgeneric destroy-component (component context)
  (:method ((component component) (context context))))
