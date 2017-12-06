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

(defmethod make-component (context component-type &rest initargs)
  (let ((qualified-type (qualify-component (core-state context)
                                           component-type)))
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

(defun qualify-component (core-state component-type)
  (declare (ignorable core-state))

  ;; iterate down the toposort and return a symbol interned in the correct
  ;; package when I find it.

  ;; TODO. CHeck this over, make it better. definite memoizztion.
  (let* ((angph (gethash 'component-package-search-order
                         (analyzed-graphs core-state)))
         (annotation (annotation angph)))

    (dolist (potential-package (toposort angph))
      (let ((potential-package-name (second potential-package)))
        #++(format t "checking for component ~A in potential-package-name ~A~%"
                   component-type potential-package-name)
        (dolist (pkg-to-search (gethash potential-package-name
                                        (pattern-matched-packages annotation)))
          #++(format t "checking for component ~A in pkg-to-search ~A~%"
                     component-type pkg-to-search)
          (multiple-value-bind (sym kind)
              (find-symbol (symbol-name component-type) pkg-to-search)
            (when (and (eq kind :external)
                       (find-class sym))
              #++(format t "XXX Found a component: pkg:[~A]:~A~%"
                         (package-name pkg-to-search) sym)
              (return-from qualify-component sym)))))))


  #++(format t "QUALIFY-COMPONENT: FALLBACK~%")
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
