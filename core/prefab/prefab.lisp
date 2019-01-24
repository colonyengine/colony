(in-package :fl.prefab)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%library :reader library
             :initarg :library)
   (%data :reader data
          :initarg :data)
   (%parse-tree :reader parse-tree
                :initform (u:dict #'equalp))
   (%root :reader root)
   (%links :reader links
           :initform (u:dict #'eq
                             :source->targets (u:dict #'equalp)
                             :target->source (u:dict #'equalp)))))

(u:define-printer (prefab stream :type t)
  (format stream "~a" (name prefab)))

(defclass node ()
  ((%name :reader name
          :initarg :name)
   (%prefab :reader prefab
            :initarg :prefab)
   (%path :reader path
          :initarg :path)
   (%options :reader options
             :initarg :options
             :initform nil)
   (%components :reader components
                :initform nil)
   (%components-table :reader components-table
                      :initform (u:dict #'eq))
   (%parent :reader parent
            :initarg :parent
            :initform nil)
   (%children :reader children
              :initform (u:dict #'equalp))))

(u:define-printer (node stream :type t)
  (format stream "~a" (path node)))

(defun split-components/children (data)
  (flet ((children-form-p (form)
           (and (listp form)
                (typep (car form) '(and (not null) (or list string))))))
    (let ((index (or (position-if #'children-form-p data)
                     (length data))))
      (values (subseq data 0 index)
              (subseq data index)))))

(defun explode-path (path)
  (u:string-split path #\/))

(defun make-node-path (parent name)
  (u:string-merge parent "/" name))

(defun make-node-path-from-parts (path-parts)
  (format nil "/~{~a~^/~}" path-parts))

(defun find-library (name)
  (u:if-found (library (u:href (fl.data:get 'prefabs) name))
              library
              (error "Prefab library ~s does not exist." name)))

(defun %find-prefab (name library)
  (let ((library (find-library library)))
    (u:href library name)))

(defun find-prefab (name library)
  (or (%find-prefab name library)
      (error "Prefab ~s not found in library ~s." name library)))

(defun %find-node (path library)
  (let* ((prefab-name (first (explode-path path)))
         (prefab (%find-prefab prefab-name library)))
    (when prefab
      (u:href (parse-tree prefab) path))))

(defun find-node (path library)
  (or (%find-node path library)
      (error "Prefab node ~s not found in library ~s." path library)))

(defun map-nodes (func node)
  (funcall func node)
  (u:do-hash-values (child (children node))
    (map-nodes func child)))

(defun parse-copy/link (library path copy-p link-p form)
  (flet ((check-source (source)
           (ensure-copy/link-source-valid path source)
           (ensure-copy/link-source-absolute path source)
           (ensure-copy/link-source-no-trailing-slash path source))
         (make-path-options (source from)
           (values path
                   (list :mode (cond (copy-p 'copy) (link-p 'link))
                         :source source
                         :from from))))
    (typecase form
      (string
       (check-source form)
       (make-path-options form library))
      (list
       (destructuring-bind (source &key (from library)) form
         (check-source source)
         (ensure-copy/link-source-string path source)
         (ensure-copy/link-library path from)
         (make-path-options source from))))))

(defun parse-path-spec (parent library path-spec)
  (labels ((check-path (path)
             (ensure-path-string path)
             (ensure-path-length parent path)
             (ensure-path-valid path)
             (ensure-path-relative path)
             (ensure-path-no-trailing-slash path))
           (direct-path (path)
             (check-path path)
             (values (make-node-path parent path)
                     (list :mode 'direct))))
    (typecase path-spec
      (string
       (direct-path path-spec))
      (list
       (destructuring-bind (target options) path-spec
         (check-path target)
         (let ((path (make-node-path parent target)))
           (ensure-path-options-plist path options)
           (ensure-path-options-valid path options)
           (destructuring-bind (&key (copy nil copy-p) (link nil link-p)) options
             (ensure-path-options-keys path options)
             (u:if-let ((copy/link-form (or copy link)))
               (parse-copy/link library path copy-p link-p copy/link-form)
               (direct-path target)))))))))

(defun make-node (prefab path)
  (symbol-macrolet ((node (u:href (parse-tree prefab) path)))
    (unless node
      (let ((name (first (last (explode-path path)))))
        (setf node (make-instance 'node
                                  :name name
                                  :prefab prefab
                                  :path path
                                  :options '(:mode direct)))))
    node))

(defun make-paths (prefab path data &optional options)
  (u:mvlet ((components children (split-components/children data))
            (node (make-node prefab path)))
    (declare (ignore components))
    (when options
      (setf (slot-value node '%options) options))
    (dolist (child children)
      (destructuring-bind (path-spec . body) child
        (u:mvlet ((path options (parse-path-spec path (library prefab) path-spec)))
          (make-paths prefab path body options))))))

(defun add-components (prefab path data)
  (u:mvlet ((components children (split-components/children data)))
    (let ((node (make-node prefab path)))
      (with-slots (%components) node
        (u:appendf %components components)
        (dolist (child children)
          (destructuring-bind (path-spec . body) child
            (let ((path (parse-path-spec path (library prefab) path-spec)))
              (add-components prefab path body))))))))

(defun make-parse-tree (prefab data)
  (with-slots (%library %name %root) prefab
    (let ((path (make-node-path nil %name)))
      (make-paths prefab path data)
      (add-components prefab path data)
      (setf %root (find-node path %library)))))

(defun expand-parse-tree-path (prefab path-parts)
  (u:when-let* ((path-parts (butlast path-parts))
                (path (format nil "/~{~a~^/~}" path-parts)))
    (make-node prefab path)
    (expand-parse-tree-path prefab path-parts)))

(defun expand-parse-tree (prefab)
  (u:do-hash-keys (path (parse-tree prefab))
    (expand-parse-tree-path prefab (explode-path path))))

(defun verify-components (prefab)
  (u:do-hash (path node (parse-tree prefab))
    (dolist (component (components node))
      (ensure-component-list component path)
      (ensure-component-form component path)
      (destructuring-bind (type options . args) component
        (ensure-component-type-symbol type path)
        (ensure-component-type-exists type path)
        (ensure-component-options-plist type options path)
        (ensure-component-options-valid type options path)
        (ensure-component-id type options path)
        (ensure-component-policy type options path)
        (ensure-component-args-plist type args path)
        (ensure-component-args-valid type args path)))))

(defun insert-missing-transforms (prefab)
  (u:do-hash (path node (parse-tree prefab))
    (with-slots (%components) node
      (unless (find 'fl.comp:transform %components :key #'car)
        (push '(fl.comp:transform (:policy old-type)) %components))
      (ensure-path-single-transform %components path))))

(defun collect-source-components (node)
  (let ((components))
    (u:do-hash (type table (components-table node))
      (u:do-hash-values (data table)
        (destructuring-bind (&key id args &allow-other-keys) data
          (push (list* type `(:id ,id) args) components))))
    components))

(defun insert-source-components (source target)
  (with-slots (%components) target
    (setf %components (append (collect-source-components source) %components))))

(defun copy-source-nodes (prefab)
  (u:do-hash (path node (parse-tree prefab))
    (destructuring-bind (&key source from &allow-other-keys) (options node)
      (when source
        (map-nodes
         (lambda (x)
           (if (string= source (path x))
               (insert-source-components x node)
               (let* ((sub-path (subseq (path x) (1+ (length source))))
                      (new-path (make-node-path path sub-path))
                      (new-node (make-node prefab new-path)))
                 (insert-source-components x new-node))))
         (find-node source from))))))

(defun make-relationships (prefab)
  (flet ((get-parent (path)
           (u:when-let ((path-parts (butlast (explode-path path))))
             (make-node-path-from-parts path-parts))))
    (with-slots (%library) prefab
      (u:do-hash (path node (parse-tree prefab))
        (u:when-let ((parent-path (get-parent path)))
          (setf (slot-value node '%parent) (find-node parent-path %library)
                (u:href (children (parent node)) path) (find-node path %library)))))))

(defun get-source-prefab (node)
  (destructuring-bind (&key source from &allow-other-keys) (options node)
    (let ((name (first (explode-path source))))
      (find-prefab name from))))

(defun make-links (prefab)
  (u:do-hash (path node (parse-tree prefab))
    (destructuring-bind (&key mode source &allow-other-keys) (options node)
      (when (eq mode 'link)
        (let* ((source-prefab (get-source-prefab node))
               (target (cons (library prefab) path))
               (links (u:href (links source-prefab))))
          (symbol-macrolet ((targets (u:href links :source->targets source)))
            (setf (u:href links :target->source target) source)
            (unless targets
              (setf targets (u:dict #'equalp)))
            (setf (u:href targets target) node)))))))

(defun remove-broken-links (prefab)
  (let ((source->targets (u:href (links prefab) :source->targets))
        (target->source (u:href (links prefab) :target->source)))
    (u:do-hash (target source target->source)
      (destructuring-bind (library . path) target
        (unless (%find-node path library)
          (remhash target target->source)
          (remhash target (u:href source->targets source))
          (unless (u:href source->targets source)
            (remhash source source->targets)))))
    (u:do-hash-keys (source source->targets)
      (unless (%find-node source (library prefab))
        (remhash source source->targets)
        (u:do-hash (k v target->source)
          (when (string= v source)
            (remhash k target->source)))))))

(defun reinitialize-prefab (prefab)
  (clrhash (parse-tree prefab))
  (parse-prefab prefab)
  prefab)

(defun update-links-recursively (prefab)
  (u:do-hash-keys (target (u:href (links prefab) :target->source))
    (destructuring-bind (library . path) target
      (let* ((name (first (explode-path path)))
             (target-prefab (find-prefab name library)))
        (reinitialize-prefab target-prefab)
        (update-links-recursively target-prefab)))))

(defun update-links (prefab)
  (make-links prefab)
  (remove-broken-links prefab)
  (update-links-recursively prefab))

(defgeneric merge-component (policy node type id args)
  (:method ((policy null) node type id args)
    (ensure-component-not-duplicate node type id)
    (merge-component 'new-type node type id args)))

(defmethod merge-component ((policy (eql 'new-type)) node type id args)
  (setf (u:href (components-table node) type id) (list :id id :policy policy :args args)))

(defmethod merge-component ((policy (eql 'old-type)) node type id args)
  (u:unless-found (components (u:href (components-table node) type id))
    (setf (u:href (components-table node) type id) (list :id id :policy policy :args args))))

(defmethod merge-component ((policy (eql 'new-args)) node type id args)
  (let* ((old-args (u:plist->hash (getf (u:href (components-table node) type id) :args)))
         (new-args (u:hash->plist (u:merge-tables old-args (u:plist->hash args)))))
    (setf (u:href (components-table node) type id) (list :id id :policy policy :args new-args))))

(defmethod merge-component ((policy (eql 'old-args)) node type id args)
  (let* ((old-args (u:plist->hash (getf (u:href (components-table node) type id) :args)))
         (new-args (u:hash->plist (u:merge-tables (u:plist->hash args) old-args))))
    (setf (u:href (components-table node) type id) (list :id id :policy policy :args new-args))))

(defun make-component-table (prefab)
  (u:do-hash-values (node (parse-tree prefab))
    (dolist (component (components node))
      (destructuring-bind (type (&key (id 0) policy) . args) component
        (unless (u:href (components-table node) type)
          (setf (u:href (components-table node) type) (u:dict #'eql)))
        (merge-component policy node type id args)))))

(defun print-prefab (name library)
  (flet ((print-line (level value)
           (format t "~a~v@<~s~>~%"
                   (make-string level :initial-element #\Space)
                   level value)))
    (map-nodes
     (lambda (x)
       (let ((level (* 3 (1- (length (explode-path (path x)))))))
         (print-line level (name x))
         (u:do-hash (type table (components-table x))
           (u:do-hash-values (data table)
             (print-line level `(,type ,@(getf data :args)))))
         (format t "~%")))
     (root (find-prefab name library)))))

(defun parse-prefab (prefab)
  (let ((success-p))
    (with-slots (%name %library %data) prefab
      (unwind-protect
           (progn
             (make-parse-tree prefab %data)
             (expand-parse-tree prefab)
             (verify-components prefab)
             (insert-missing-transforms prefab)
             (copy-source-nodes prefab)
             (make-component-table prefab)
             (make-relationships prefab)
             (update-links prefab)
             (setf success-p t))
        (unless success-p
          (remhash %name (u:href (fl.data:get 'prefabs) %library)))))))

(defun make-prefab (name library data)
  (let ((prefab (or (%find-prefab name library)
                    (make-instance 'prefab
                                   :name name
                                   :library library))))
    (with-slots (%data %parse-tree) prefab
      (setf %data data)
      (clrhash %parse-tree))
    prefab))

(defmacro define-prefab (name (&optional library) &body body)
  (let* ((libraries '(fl.data:get 'prefabs))
         (prefabs `(u:href ,libraries ',library)))
    (u:with-unique-names (prefab)
      `(progn
         (ensure-prefab-name-string ',name)
         (ensure-prefab-name-valid ',name)
         (ensure-prefab-library-set ',name ',library)
         (ensure-prefab-library-symbol ',name ',library)
         (unless ,libraries
           (fl.data:set 'prefabs (u:dict #'eq)))
         (unless ,prefabs
           (setf ,prefabs (u:dict #'equalp)))
         (let ((,prefab (make-prefab ',name ',library ',body)))
           (setf (u:href ,prefabs ',name) ,prefab)
           (parse-prefab ,prefab))
         (export ',library)))))

(define-prefab "foo" (test)
  ("a"
   ("b"
    ("c"))))

(define-prefab "foo2" (test)
  ("d"
   (("e" (:link "/foo/a"))
    ("f"))))
