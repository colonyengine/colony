(in-package :fl.core)

(defvar *registered-components* nil)

(defclass component ()
  ((%type :reader component-type
          :initarg :type)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%actor :accessor actor
           :initarg :actor)
   (%initializer-thunk :accessor initializer-thunk
                       :initarg :initializer-thunk
                       :initform nil)))

(defmacro define-component (name super-classes &body slots)
  `(progn
     (unless (char= (aref (symbol-name ',name) 0) #\$)
       (error "Component names must be prefixed with a '$' character."))
     (defclass ,name
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
                   `(:type ,type))))))
     (pushnew ',name *registered-components*)))

(defun register-component (component-type)
  (pushnew (cons (symbol-package component-type) (symbol-name component-type))
           *registered-components*
           :test #'equalp))

(defmethod make-component (component-type &rest initargs)
  (let ((qualified-type (qualify-component component-type)))
    (apply #'make-instance qualified-type :type qualified-type initargs)))

(defun realize-component (core-state component)
  (when-let ((thunk (initializer-thunk component)))
    (funcall thunk)
    (setf (initializer-thunk component) nil))
  (setf (state component) :active
        (type-table
         (canonicalize-component-type (component-type component) core-state)
         (component-active-by-type-view core-state))
        component))

(defun realize-components (core-state component-table)
  (maphash
   (lambda (k component)
     (declare (ignore k))
     (realize-component core-state component))
   component-table))

(defun qualify-component (component-type)
  (or
   (find-if
    (lambda (x)
      (string= (symbol-name x) (symbol-name component-type)))
    *registered-components*)
   (error "~a is not a known component type." component-type)))

;; The Component Protocol.
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
