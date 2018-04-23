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

;;; BEGIN User API Component Protocol

;; TODO: Make this empty one export properly from fl.core so we always see it.
(defgeneric shared-storage-metadata (component-name)
  (:method (component-name)))

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

;;; END User API Component Protocol

(defun %generate-component-shared-keys (slots)
  (let ((shared-keys))
    (mapcar
     (lambda (x)
       (destructuring-bind (slot-name &key shared &allow-other-keys) x
         (when shared
           (push slot-name shared-keys))))
     slots)
    shared-keys))

(defun %generate-component-slot-forms (slots)
  (loop :for slot :in slots
        :collect
        (destructuring-bind (slot-name &key default type &allow-other-keys) slot
          (append
           `(,(au:symbolicate '% slot-name)
             :accessor ,slot-name
             :initarg ,(au:make-keyword slot-name)
             :initform ,default)
           (when type
             `(:type ,type))))))

(defun %generate-shared-storage-slot-forms (slots)
  (loop :for slot :in slots
        :collect
        (destructuring-bind (slot-name &key &allow-other-keys) slot
          `(,(au:symbolicate '% slot-name)
            :reader ,slot-name
            :initarg ,(au:make-keyword slot-name)))))

(defun %generate-shared-storage-initargs (slots)
  (loop :for slot :in slots
        :append
        (destructuring-bind (slot-name &key &allow-other-keys) slot
          (list (au:make-keyword slot-name) slot-name))))

(defmacro define-component (name super-classes &body body)
  (let* ((entry-symbol (au:symbolicate name '-shared-storage-entry))
         ;; NOTE: Can't use destructuring-bind because these forms might
         ;; not actually be present in the form so there would be a mismatch
         ;; or arguments for destructuring. We rely on the fact these accessors
         ;; return NIL for things beyond the end of the body.
         (slots (first body))
         (shared-storage-metadata (second body))
         (shared-keys (%generate-component-shared-keys slots)))
    (au:with-unique-names (store-var entry-var)
      `(progn
         (defclass ,name (,@(append (unless super-classes '(component)) super-classes))
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
         (defmethod ,(intern (symbol-name 'shared-storage-metadata)
                             :fl.core)
             ((component-name (eql ',name)))

           (declare (ignore component-name))
           ;; TODO: for now, a hack, but make this more efficient in what it
           ;; returns. Namely, a static hash table or something related to the
           ;; data so it is easier to look up.
           ;;
           ;; TODO: Technically, it would be awesome if this could search the
           ;; package searching graph to figure out the info, but it might be
           ;; impossible here. However, the caller of this, SS-HREF, MIGHT
           ;; always have a context around, so they could qualify the
           ;; component-name!
           ',shared-storage-metadata)

         ;; TODO: this stuff below gets trashed (and the macro code to make
         ;; it)when the new shared storage works.
         (defclass ,(au:symbolicate name '-shared-storage) ()
           ((%cache :accessor cache :initform (au:dict #'equalp))))
         (defclass ,entry-symbol ()
           ,(%generate-shared-storage-slot-forms slots))
         (defun ,(au:symbolicate 'make- name '-shared-storage-entry) ,(mapcar #'first slots)
           (make-instance ',entry-symbol ,@(%generate-shared-storage-initargs slots)))
         (defun ,entry-symbol (,store-var ,@shared-keys)
           (au:href (cache ,store-var) (list ,@shared-keys)))
         (defun (setf ,entry-symbol) (,entry-var ,store-var ,@shared-keys)
           (setf (au:href (cache ,store-var) (list ,@shared-keys)) ,entry-var))))))

(defmethod make-component (component-type context &rest initargs)
  (let ((qualified-type (qualify-component (core-state context) component-type)))
    (apply #'make-instance qualified-type :type qualified-type initargs)))

(defun %get-component-precedence-list (component-type)
  (loop :for class :in (c2mop:class-precedence-list (find-class component-type))
        :for name = (class-name class)
        :until (eq name 'component)
        :collect name))

(defun qualify-component (core-state component-type)
  "Determine if the symbol COMPONENT-TYPE represents a real component. If so, return the
package-qualified symbol of the actual type that is acceptable to pass to MAKE-INSTANCE. This
qualification algorithm follows the search order defined in the graph category
COMPONENT-PACKAGE-ORDER."
  (let ((search-table (component-search-table (tables core-state)))
        (component-type/class (find-class component-type nil))
        (base-component-type/class (find-class 'fl.core:component)))
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
      (type-table-drop
       component component-type (component-preinit-by-type-view %tables))
      (setf (type-table component-type (component-init-by-type-view %tables)) component))))

(defun component/init->active (core-state component)
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (type-table-drop
       component component-type (component-init-by-type-view %tables))
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
      (type-table-drop
       component component-type (component-destroy-by-type-view %tables))
      (unless (type-table-drop
               component component-type (component-active-by-type-view %tables))
        (type-table-drop
         component component-type (component-preinit-by-type-view %tables))))))

(defun component/destroy->released (core-state component)
  (let ((component-type (canonicalize-component-type (component-type component) core-state)))
    (with-slots (%tables) core-state
      (type-table-drop
       component component-type (component-destroy-by-type-view %tables))
      (detach-component (actor component) component))))

(defun component/countdown-to-destruction (core-state component)
  (when (plusp (ttl component))
    (decf (ttl component) (box.frame:frame-time (display core-state)))))
