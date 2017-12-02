(in-package :first-light)

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

(defmethod make-component (component-type &rest initargs)
  (apply #'make-instance component-type :type component-type initargs))

(defun realize-component (core-state component)
  (when-let ((thunk (initializer-thunk component)))
    (funcall thunk)
    (setf (initializer-thunk component) nil))

  (setf (state component) :active)

  (setf (type-table
         (canonicalize-component-type (component-type component) core-state)
         (component-active-by-type-view core-state))
        component))


(defun realize-components (core-state component-table)
  "For all component values in the COMPONENT-HT hash table, run their
initialize-thunks, set them :active, and put them into the active component
view."
  (maphash
   (lambda (k component)
     (declare (ignore k))
     (realize-component core-state component))
   component-table))


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
