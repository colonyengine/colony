(in-package :%first-light)

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
        (let ((graph (au:href (analyzed-graphs core-state)
                              'component-package-order)))
          (dolist (potential-package (toposort graph))
            (let ((potential-package-name (second potential-package)))
              (dolist (pkg-to-search (au:href (pattern-matched-packages
                                               (annotation graph))
                                              potential-package-name))
                (multiple-value-bind (symbol kind)
                    (find-symbol (symbol-name component-type) pkg-to-search)
                  (when (and (eq kind :external)
                             (find-class symbol nil))
                    (setf (au:href search-table component-type) symbol)
                    (return-from qualify-component symbol)))))))
        component-type)))

(defmethod make-component (context component-type &rest initargs)
  (au:if-let ((qualified-type (qualify-component (core-state context)
                                                 component-type)))
    (apply #'make-instance qualified-type
           :type qualified-type
           :context context initargs)
    (error "Could not qualify the component type ~s." component-type)))

(defun get-computed-component-precedence-list (component-type)
  (au:when-let ((class (find-class component-type nil)))
    (loop :for class :in (c2mop:compute-class-precedence-list class)
          :for name = (class-name class)
          :until (eq name 'component)
          :when (subtypep name 'component)
            :collect name)))

(defun component/preinit->init (component)
  (au:when-let ((thunk (initializer-thunk component)))
    (funcall thunk)
    (setf (initializer-thunk component) nil))
  (let* ((core-state (core-state (context component)))
         (component-type (canonicalize-component-type (component-type component)
                                                      core-state)))
    (with-slots (%tables) core-state
      (type-table-drop component component-type (component-preinit-by-type-view
                                                 %tables))
      (setf (type-table component-type (component-init-by-type-view %tables))
            component))))

(defun component/init->active (component)
  (let* ((core-state (core-state (context component)))
         (component-type (canonicalize-component-type (component-type component)
                                                      core-state)))
    (with-slots (%tables) core-state
      (type-table-drop component component-type (component-init-by-type-view
                                                 %tables))
      (setf (state component) :active
            (type-table component-type (component-active-by-type-view %tables))
            component))))

(defmethod destroy ((thing component) &key (ttl 0))
  (let ((core-state (core-state (context thing))))
    (setf (ttl thing) (if (minusp ttl) 0 ttl)
          (au:href (component-predestroy-view
                    (tables core-state)) thing)
          thing)))

(defun component/init-or-active->destroy (component)
  (let* ((core-state (core-state (context component)))
         (component-type (canonicalize-component-type (component-type component)
                                                      core-state)))
    (unless (plusp (ttl component))
      (with-slots (%tables) core-state
        (setf (state component) :destroy
              (type-table component-type
                          (component-destroy-by-type-view %tables))
              component)
        (remhash component (component-predestroy-view %tables))
        (unless (type-table-drop component
                                 component-type
                                 (component-active-by-type-view %tables))
          (type-table-drop component
                           component-type
                           (component-preinit-by-type-view %tables)))))))

(defun component/destroy->released (component)
  (let* ((core-state (core-state (context component)))
         (component-type (canonicalize-component-type
                          (component-type component) core-state)))
    (type-table-drop component
                     component-type
                     (component-destroy-by-type-view (tables core-state)))
    (detach-component (actor component) component)))

(defun component/countdown-to-destruction (component)
  (when (plusp (ttl component))
    (decf (ttl component) (frame-time (context component)))))

(defun enqueue-attach-event (component actor)
  (fl.dst:qpush (attach/detach-event-queue component) (list :attached actor)))

(defun enqueue-detach-event (component actor)
  (fl.dst:qpush (attach/detach-event-queue component) (list :detached actor)))

(defun dequeue-attach/detach-event (component)
  ;; NOTE: Returns NIL, which is not in our domain, when empty.
  (fl.dst:qpop (attach/detach-event-queue component)))

(defun component/invoke-attach/detach-events (component)
  (loop :for (event-kind actor) = (fl.dst:qpop (attach/detach-event-queue
                                                component))
        :while event-kind
        :do (ecase event-kind
              (:attached
               (on-component-attach component actor))
              (:detached
               (on-component-detach component actor)))))

;;; User protocol

(defgeneric shared-storage-metadata (component-name &optional namespace)
  (:method ((component-name symbol) &optional namespace)
    (declare (ignore namespace))))

(defgeneric on-component-initialize (component)
  (:method ((self component))))

(defgeneric on-component-attach (component actor)
  (:method ((self component) actor)))

(defgeneric on-component-detach (component actor)
  (:method ((self component) actor)))

(defgeneric on-component-physics-update (component)
  (:method ((self component))))

(defgeneric on-component-update (component)
  (:method ((self component))))

(defgeneric on-component-render (component)
  (:method ((self component))))

(defgeneric on-component-destroy (component)
  (:method ((self component))))
