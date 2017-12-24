(in-package :fl.core)

(defclass component ()
  ((%type :reader component-type
          :initarg :type)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%actor :accessor actor
           :initarg :actor
           :initform nil)
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)
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

(defmethod make-component (component-type context &rest initargs)
  (let ((qualified-type (qualify-component (core-state context)
                                           component-type)))
    (apply #'make-instance qualified-type :type qualified-type initargs)))

(defun qualify-component (core-state component-type)
  "Determine if the symbol COMPONENT-TYPE represents a real component. If so,
return the package-qualified symbol of the actual type that is acceptable to
pass to MAKE-INSTANCE. This qualification algorithm follows the search order
defined in the graph category COMPONENT-PACKAGE-SEARCH-ORDER."
  ;; Do a fast lookup in a memoization table first.
  (multiple-value-bind (pkg-sym presentp)
      (gethash component-type (component-search-table core-state))
    (when presentp
      (return-from qualify-component pkg-sym)))
  (let* ((component-type/class (find-class component-type nil))
         (base-component-type/class (find-class 'fl.core:component)))
    ;; If the symbol doesn't already denote a component class in the package it
    ;; is in, then look it up.
    (if (or
         ;; the component-type isn't even a class in whatever package the reader
         ;; found it in.
         (null component-type/class)
         ;; The component-type is a class, but not a subclass of component.
         (not (subtypep (class-name component-type/class)
                        (class-name base-component-type/class))))
        (let* ((graph (gethash 'component-package-search-order
                               (analyzed-graphs core-state)))
               (annotation (annotation graph)))
          ;; Iterate down the toposort and return the true component class name
          ;; symbol interned in the correct package when I find it.
          (dolist (potential-package (toposort graph))
            (let ((potential-package-name (second potential-package)))
              (dolist (pkg-to-search
                       (gethash potential-package-name
                                (pattern-matched-packages annotation)))
                (multiple-value-bind (sym kind)
                    (find-symbol (symbol-name component-type) pkg-to-search)
                  (when (and (eq kind :external) (find-class sym nil))
                    ;; don't forget to memoize it!
                    (setf (gethash component-type
                                   (component-search-table core-state))
                          sym)
                    (return-from qualify-component sym)))))))
        ;; Otherwise, use the symbol itself, because the user qualified it or it
        ;; already represents an applicable component in the home package.
        component-type)))



(defun component/preinit->init (core-state component)
  #++(format t "component/preinit->init: ~A~%" component)

  (when-let ((thunk (initializer-thunk component)))
    (funcall thunk)
    (setf (initializer-thunk component) nil))

  (let ((canonicalized-component-type (canonicalize-component-type
                                       (component-type component)
                                       core-state)))
    ;; remove it from the pre-init table.
    (remhash component
             (type-table canonicalized-component-type
                         (component-preinitialize-by-type-view core-state)))

    ;; move it into the init table.
    (setf (type-table
           canonicalized-component-type
           (component-initialize-by-type-view core-state))
          component)))

(defun component/init->active (core-state component)
  #++(format t "component/init->active: ~A~%" component)

  (let ((canonicalized-component-type (canonicalize-component-type
                                       (component-type component)
                                       core-state)))
    ;; remove it from the init table.
    (remhash component
             (type-table canonicalized-component-type
                         (component-initialize-by-type-view core-state)))

    ;; move it into the active table.
    (setf (state component) :active
          (type-table
           canonicalized-component-type
           (component-active-by-type-view core-state))
          component)))


(defmethod destroy ((thing component) (context context) &key (ttl 0))
  (let ((core-state (core-state context)))
    (setf (ttl thing) (if (< ttl 0) 0 ttl))
    (setf (gethash thing (component-predestroy-view core-state)) thing)))

(defun component/init-or-active->destroy (core-state component)
  (let ((canonicalized-component-type (canonicalize-component-type
                                       (component-type component)
                                       core-state)))
    ;; 1. Add it to destroy state.
    (setf (type-table canonicalized-component-type
                      (component-destroy-by-type-view core-state))
          component)

    ;; 2. Set its state to destroying.
    (setf (state component) :destroy)

    ;; 3. remove it from predestroy state (it may not be there, that's ok).
    (remhash component (component-predestroy-view core-state))

    ;; 4a. remove it from active state, OR
    ;; 4b. remove it from init state.
    ;; It will be in one of those two.
    (unless (remhash component
                     (type-table canonicalized-component-type
                                 (component-active-by-type-view core-state)))
      (remhash component
               (type-table canonicalized-component-type
                           (component-preinitialize-by-type-view
                            core-state))))))

(defun component/destroy->released (core-state component)
  (let ((canonicalized-component-type (canonicalize-component-type
                                       (component-type component)
                                       core-state)))

    ;; 1. Remove it from destroy table.
    ;; At this point, no core-state tables should have a reference to this
    ;; component!
    (remhash component
             (type-table canonicalized-component-type
                         (component-destroy-by-type-view core-state)))

    ;; 2. Remove it from actor.
    ;; At this point, core-state releases any knowledge of any references to
    ;; this component. The USER CODE may still have references, and it is up
    ;; to them to check if they still have a valid component reference
    ;; (by ensuring the state is not :destroyed) and it is up to them to deal
    ;; with it.
    (remove-component (actor component) component)))

(defun component/countdown-to-destruction (core-state component)
  (when (> (ttl component) 0)
    (decf (ttl component) (box.fm:frame-time (display core-state)))))


;;; User API Component Protocol

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
