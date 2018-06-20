(in-package :%fl)

(defclass component ()
  ((%context :reader context
             :initarg :context
             :initform nil)
   (%type :reader component-type
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

(defun %generate-component-slot-forms (slots)
  (loop :for slot :in slots
        :collect
        (destructuring-bind (slot-name &key default allocation type &allow-other-keys) slot
          (append
           `(,(au:symbolicate '% slot-name)
             :accessor ,slot-name
             :initarg ,(au:make-keyword slot-name)
             :initform ,default)
           (when type
             `(:type ,type))
           (when allocation
             `(:allocation ,allocation))))))

(defmacro define-component (name super-classes &body body)
  (let* (;; NOTE: Can't use destructuring-bind because these forms might
         ;; not actually be present in the form so there would be a mismatch
         ;; or arguments for destructuring. We rely on the fact these accessors
         ;; return NIL for things beyond the end of the body.
         (slots (first body))
         (shared-storage-metadata (second body)))
    `(progn
       ;; It is ok if COMPONENT is added more than once in the heirarchy
       ;; because since it is the same type, there will only be one
       ;; version of it in the type heirarchy for the resultant class.
       (defclass ,name (,@(append '(component) super-classes))
         ,(%generate-component-slot-forms slots))

       ;; Create a method which provides the shared-storage-metadata for this
       ;; component-name.
       ;;
       ;; TODO: It is possible to make an individual function concerning this
       ;; concept like shared-storage-metadata/NAME. It would provide a
       ;; non-expensive function to get the data, but one for each
       ;; component. So the user API is a bit wonky. However, the actual game
       ;; dev probably wouldn't see this function too often. Still, pacakges
       ;; make it interesting....
       (defmethod ,(intern (symbol-name 'shared-storage-metadata) :%fl)
           ((component-name (eql ',name)) &optional namespace)

         (declare (ignore component-name))
         ;; TODO: for now, a hack, but make this more efficient in what it
         ;; returns. Namely, a static hash table or something related to the
         ;; data so it is easier to look up.
         (let ((ss-meta ',shared-storage-metadata))
           (if namespace
               ;; TODO: make this better.
               (find namespace ss-meta :key #'first)
               ss-meta))))))

(defmethod make-component (component-type context &rest initargs)
  (let ((qualified-type (qualify-component (core-state context) component-type)))
    (apply #'make-instance qualified-type :type qualified-type :context context initargs)))

(defun %get-computed-component-precedence-list (component-type)
  ;; NOTE: We may very well be asking for classes that have not been finalized
  ;; because we haven't yet (or might not ever) call make-instance on them.
  ;; Hence we will compute right now the class precedence for it.
  ;; TODO: Fix this when FIND-CLASS returns NIL too.
  (loop :for class :in (c2mop:compute-class-precedence-list
                        (find-class component-type nil))
        :for name = (class-name class)
        :until (eq name 'component)
        :collect name))

(defun qualify-component (core-state component-type)
  "This function tries to resolve the COMPONENT-TYPE symbol into a potentially
different packaged symbol of the same name that corresponds to a component
definition in that package. The packages are searched in the order they are are
defined in a toposort of the graph category COMPONENT-PACKAGE-ORDER. The result
should be a symbol suitable for MAKE-INSTANCE in all cases, but in the case of
mixin superclasses, it might not be desireable.

NOTE: If the component-type is a mixin class/component that is a superclass to a
component, then the first external to the package superclass definition found in
the package search order will be returned as the package qualified symbol.

NOTE: This function can not confirm that a symbol is a component defined by
DEFINE-COMPONENT. It can only confirm that the symbol passed to it is a
superclass of a DEFINE-COMPONENT form (up to but not including the COMPONENT
superclass type all components have), or a component created by the
DEFINE-COMPONENT form."
  (let ((search-table (component-search-table (tables core-state)))
        (component-type/class (find-class component-type nil))
        (base-component-type/class (find-class '%fl:component)))
    (au:when-found (pkg-symbol (au:href search-table component-type))
      (return-from qualify-component pkg-symbol))
    (if (or (null component-type/class)
            (not (subtypep (class-name component-type/class)
                           (class-name base-component-type/class))))
        (let ((graph (au:href (analyzed-graphs core-state) 'component-package-order)))
          (dolist (potential-package (toposort graph))
            (let ((potential-package-name (second potential-package)))
              (dolist (pkg-to-search
                       (au:href (pattern-matched-packages (annotation graph))
                                potential-package-name))
                (multiple-value-bind (symbol kind)
                    (find-symbol (symbol-name component-type) pkg-to-search)
                  (when (and (eq kind :external)
                             (find-class symbol nil))
                    (setf (au:href search-table component-type) symbol)
                    (return-from qualify-component symbol)))))))
        component-type)))

(defun component/preinit->init (core-state component)
  (au:when-let ((thunk (initializer-thunk component)))
    (funcall thunk)
    (setf (initializer-thunk component) nil))
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (type-table-drop component component-type (component-preinit-by-type-view %tables))
      (setf (type-table component-type (component-init-by-type-view %tables)) component))))

(defun component/init->active (core-state component)
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (type-table-drop component component-type (component-init-by-type-view %tables))
      (setf (state component) :active
            (type-table component-type (component-active-by-type-view %tables)) component))))

(defmethod destroy ((thing component) (context context) &key (ttl 0))
  (let ((core-state (core-state context)))
    (setf (ttl thing) (if (minusp ttl) 0 ttl)
          (au:href (component-predestroy-view (tables core-state)) thing) thing)))

(defun component/init-or-active->destroy (core-state component)
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (setf (state component) :destroy
            (type-table component-type (component-destroy-by-type-view %tables)) component)
      (type-table-drop component component-type (component-destroy-by-type-view %tables))
      (unless (type-table-drop component component-type (component-active-by-type-view %tables))
        (type-table-drop component component-type (component-preinit-by-type-view %tables))))))

(defun component/destroy->released (core-state component)
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (type-table-drop component component-type (component-destroy-by-type-view (tables core-state)))
    (detach-component (actor component) component)))

(defun component/countdown-to-destruction (core-state component)
  (when (plusp (ttl component))
    (decf (ttl component) (frame-time (context core-state)))))

;;; User protocol

(defgeneric shared-storage-metadata (component-name &optional namespace)
  (:method ((component-name symbol) &optional namespace)
    (declare (ignore namespace))))

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
