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
     (defclass ,name (,@(append (unless super-classes '(component)) super-classes))
       ,(loop :for slot :in slots
              :collect
              (destructuring-bind (slot-name slot-value &key type) slot
                (append
                 `(,(alexandria:symbolicate '% slot-name)
                   :accessor ,slot-name
                   :initarg ,(alexandria:make-keyword slot-name)
                   :initform ,slot-value)
                 (when type
                   `(:type ,type))))))))

(defmethod make-component (component-type context &rest initargs)
  (let ((qualified-type (qualify-component (core-state context) component-type)))
    (apply #'make-instance qualified-type :type qualified-type initargs)))

(defun qualify-component (core-state component-type)
  "Determine if the symbol COMPONENT-TYPE represents a real component. If so, return the
package-qualified symbol of the actual type that is acceptable to pass to MAKE-INSTANCE. This
qualification algorithm follows the search order defined in the graph category
COMPONENT-PACKAGE-ORDER."
  (let ((search-table (component-search-table (tables core-state))))
    (multiple-value-bind (pkg-sym presentp) (gethash component-type search-table)
      (when presentp
        (return-from qualify-component pkg-sym)))
    (let ((component-type/class (find-class component-type nil))
          (base-component-type/class (find-class 'fl.core:component)))
      (if (or (null component-type/class)
              (not (subtypep (class-name component-type/class)
                             (class-name base-component-type/class))))
          (let* ((graph (gethash 'component-package-order (analyzed-graphs core-state)))
                 (annotation (annotation graph)))
            (dolist (potential-package (toposort graph))
              (let ((potential-package-name (second potential-package)))
                (dolist (pkg-to-search
                         (gethash potential-package-name (pattern-matched-packages annotation)))
                  (multiple-value-bind (symbol kind)
                      (find-symbol (symbol-name component-type) pkg-to-search)
                    (when (and (eq kind :external)
                               (find-class symbol nil))
                      (setf (gethash component-type search-table) symbol)
                      (return-from qualify-component symbol)))))))
          component-type))))

(defun component/preinit->init (core-state component)
  (alexandria:when-let ((thunk (initializer-thunk component)))
    (funcall thunk)
    (setf (initializer-thunk component) nil))
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (remhash component (type-table component-type (component-preinit-by-type-view %tables)))
      (setf (type-table component-type (component-init-by-type-view %tables)) component))))

(defun component/init->active (core-state component)
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (remhash component (type-table component-type (component-init-by-type-view %tables)))
      (setf (state component) :active
            (type-table component-type (component-active-by-type-view %tables)) component))))

(defmethod destroy ((thing component) (context context) &key (ttl 0))
  (let ((core-state (core-state context)))
    (setf (ttl thing) (if (< ttl 0) 0 ttl)
          (gethash thing (component-predestroy-view (tables core-state))) thing)))

(defun component/init-or-active->destroy (core-state component)
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (setf (state component) :destroy
            (type-table component-type (component-destroy-by-type-view %tables)) component)
      (remhash component (component-predestroy-view %tables))
      (unless (remhash component (type-table component-type (component-active-by-type-view %tables)))
        (remhash component (type-table component-type (component-preinit-by-type-view %tables)))))))

(defun component/destroy->released (core-state component)
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (remhash component (type-table component-type (component-destroy-by-type-view %tables)))
      (detach-component (actor component) component))))

(defun component/countdown-to-destruction (core-state component)
  (when (plusp (ttl component))
    (decf (ttl component) (box.frame:frame-time (display core-state)))))

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
