(in-package :fl.core)

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
                   `(:type ,type))))))))

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
  "Determine if the symbol COMPONENT-TYPE represents a real component. If
so, return the package-qualified symbol of the actual type that is acceptable
to pass to MAKE-INSTANCE. This qualification algorithm follows the search order
defined in the graph category COMPONENT-PACKAGE-SEARCH-ORDER."
  (declare (ignorable core-state))

  ;; 1) Do a fast lookup in a memoization table first...
  (multiple-value-bind (pkg-sym presentp)
      (gethash component-type (component-search-table core-state))
    (when presentp
      (return-from qualify-component pkg-sym)))

  (let* ((component-type/class (find-class component-type nil))
         (base-component-type/class (find-class 'fl.core:component)))

    ;; 2) If the symbol doesn't already denote a component class in the package
    ;; it is in, then look it up.
    (if (or
         ;; the component-type isn't even a class in whatever package the reader
         ;; found it in.
         (null component-type/class)
         ;; The component-type is a class, but not a subclass of component
         (not (subtypep (class-name component-type/class)
                        (class-name base-component-type/class))))

        (let* ((angph (gethash 'component-package-search-order
                               (analyzed-graphs core-state)))
               (annotation (annotation angph)))
          ;; Iterate down the toposort and return the true component class name
          ;; symbol interned in the correct package when I find it.
          (dolist (potential-package (toposort angph))
            (let ((potential-package-name (second potential-package)))
              (dolist (pkg-to-search
                       (gethash potential-package-name
                                (pattern-matched-packages annotation)))
                (multiple-value-bind (sym kind)
                    (find-symbol (symbol-name component-type) pkg-to-search)
                  (when (and (eq kind :external) (find-class sym nil))
                    ;; do't forget to memoize it!
                    (setf (gethash component-type
                                   (component-search-table core-state))
                          sym)
                    (return-from qualify-component sym)))))))

        ;; Otherwise, use the symbol itself, cause the user qualified it or
        ;; it already represents an applicable component in the home package.
        component-type)))



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
