(in-package :first-light.prefab)

(defun parse-prefab (prefab)
  (let (success-p)
    (with-slots (%name %library %data) prefab
      (unwind-protect
           (progn
             (make-parse-tree prefab %data)
             (verify-components prefab)
             (insert-missing-transforms prefab)
             (copy-source-nodes prefab)
             (make-relationships prefab)
             (make-component-table prefab)
             (update-links prefab)
             (setf success-p t))
        (unless success-p
          (remhash %name (au:href (fl.data:get 'prefabs) %library)))))))

(defun parse-copy/link (library id display-id path copy-p link-p form policy)
  (flet ((check-source (source)
           (ensure-copy/link-source-valid path source)
           (ensure-copy/link-source-absolute path source)
           (ensure-copy/link-source-no-trailing-slash path source))
         (make-path-options (source from)
           (values path
                   id
                   display-id
                   (list :mode (cond (copy-p 'copy) (link-p 'link))
                         :policy policy
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
             (ensure-path-no-trailing-slash path)
             (ensure-path-not-parent path))
           (direct-path (path &key id display-id policy)
             (check-path path)
             (values (make-node-path parent path)
                     id
                     display-id
                     (list :mode 'direct :policy policy))))
    (typecase path-spec
      (string
       (direct-path path-spec :display-id path-spec))
      (list
       (destructuring-bind (target . options) path-spec
         (check-path target)
         (let ((path (make-node-path parent target)))
           (ensure-path-options-plist path options)
           (ensure-path-options-valid path options)
           (destructuring-bind (&key id (display-id target) (copy nil copy-p)
                                  (link nil link-p) policy)
               options
             (ensure-path-options-keys path options)
             (au:if-let ((copy/link-form (or copy link)))
               (parse-copy/link
                library id display-id path copy-p link-p copy/link-form policy)
               (direct-path target :id id
                                   :display-id display-id
                                   :policy policy)))))))))

(defun make-node (prefab path &optional options)
  (symbol-macrolet ((node (au:href (parse-tree prefab) path)))
    (unless node
      (let ((name (first (last (explode-path path))))
            (options (if options
                         (append options (list :mode :direct))
                         (list :mode :direct))))
        (setf node (make-instance 'node
                                  :name name
                                  :prefab prefab
                                  :path path
                                  :options options))))
    node))

(defun make-parse-tree (prefab data)
  (with-slots (%library %name %root %parse-tree) prefab
    (labels ((%make-nodes (parent data)
               (dolist (node-spec data)
                 (au:mvlet*
                     ((name components children (split-spec node-spec))
                      (path id display-id options
                            (parse-path-spec parent %library name)))
                   (with-slots (%id %display-id %options %components)
                       (make-node prefab path options)
                     (setf %id id
                           %display-id display-id
                           %options options)
                     (au:appendf %components components)
                     (%make-nodes path children)))))
             (%expand-path (parts)
               (au:when-let* ((parts (butlast parts))
                              (path (format nil "/~{~a~^/~}" parts)))
                 (make-node prefab path)
                 (%expand-path parts))))
      (let ((root-path (make-node-path nil %name)))
        (%make-nodes nil data)
        (setf %root (find-node root-path %library))
        (au:do-hash-keys (path %parse-tree)
          (%expand-path (explode-path path)))))))

(defun verify-components (prefab)
  (au:do-hash (path node (parse-tree prefab))
    (dolist (component (components node))
      (ensure-component-list component path)
      (ensure-component-form component path)
      (destructuring-bind (type options . args) component
        (ensure-component-type-symbol type path)
        (ensure-component-type-exists type path)
        (ensure-component-options-plist type options path)
        (ensure-component-options-valid type options path)
        (ensure-component-policy type options path)
        (ensure-component-args-plist type args path)
        (ensure-component-args-valid type args path)))))

(defun insert-missing-transforms (prefab)
  (au:do-hash (path node (parse-tree prefab))
    (with-slots (%components) node
      (unless (find 'fl.comp:transform %components :key #'car)
        (push '(fl.comp:transform (:policy :old-type)) %components))
      (ensure-path-single-transform %components path))))

(defun collect-source-components (node)
  (let (components)
    (au:do-hash (type table (components-table node))
      (au:do-hash-values (data table)
        (destructuring-bind (&key merge-id args &allow-other-keys) data
          (push `(,type (:merge-id ,merge-id) ,@args) components))))
    components))

(defun insert-source-components (source target)
  (with-slots (%components) target
    (setf %components (append (collect-source-components source) %components))))

(defun copy-source-nodes (prefab)
  (au:do-hash (path node (parse-tree prefab))
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
           (au:when-let ((path-parts (butlast (explode-path path))))
             (make-node-path-from-parts path-parts))))
    (with-slots (%library) prefab
      (au:do-hash (path node (parse-tree prefab))
        (au:when-let ((parent-path (get-parent path)))
          (setf (slot-value node '%parent) (find-node parent-path %library)
                (au:href (children (parent node)) path)
                (find-node path %library)))))))

(defun get-source-prefab (node)
  (destructuring-bind (&key source from &allow-other-keys) (options node)
    (let ((name (first (explode-path source))))
      (find-prefab name from))))

(defun make-links (prefab)
  (au:do-hash (path node (parse-tree prefab))
    (destructuring-bind (&key mode source &allow-other-keys) (options node)
      (when (eq mode 'link)
        (let* ((source-prefab (get-source-prefab node))
               (target (cons (library prefab) path))
               (links (au:href (links source-prefab))))
          (symbol-macrolet ((targets (au:href links :source->targets source)))
            (setf (au:href links :target->source target) source)
            (unless targets
              (setf targets (au:dict #'equalp)))
            (setf (au:href targets target) node)))))))

(defun remove-broken-links (prefab)
  (let ((source->targets (au:href (links prefab) :source->targets))
        (target->source (au:href (links prefab) :target->source)))
    (au:do-hash (target source target->source)
      (destructuring-bind (library . path) target
        (unless (%find-node path library)
          (remhash target target->source)
          (remhash target (au:href source->targets source))
          (unless (au:href source->targets source)
            (remhash source source->targets)))))
    (au:do-hash-keys (source source->targets)
      (unless (%find-node source (library prefab))
        (remhash source source->targets)
        (au:do-hash (k v target->source)
          (when (string= v source)
            (remhash k target->source)))))))

(defun reinitialize-prefab (prefab)
  (clrhash (parse-tree prefab))
  (parse-prefab prefab)
  prefab)

(defun update-links-recursively (prefab)
  (au:do-hash-keys (target (au:href (links prefab) :target->source))
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
    (merge-component :new-type node type id args)))

(defmethod merge-component ((policy (eql :new-type)) node type id args)
  (setf (au:href (components-table node) type id)
        (list :merge-id id :policy policy :args args)))

(defmethod merge-component ((policy (eql :old-type)) node type id args)
  (au:unless-found (components (au:href (components-table node) type id))
    (setf (au:href (components-table node) type id)
          (list :merge-id id :policy policy :args args))))

(defmethod merge-component ((policy (eql :new-args)) node type id args)
  (let* ((old-args (au:plist->hash
                    (getf (au:href (components-table node) type id) :args)))
         (new-args (au:hash->plist
                    (au:merge-tables old-args (au:plist->hash args)))))
    (setf (au:href (components-table node) type id)
          (list :merge-id id :policy policy :args new-args))))

(defmethod merge-component ((policy (eql :old-args)) node type id args)
  (let* ((old-args (au:plist->hash
                    (getf (au:href (components-table node) type id) :args)))
         (new-args (au:hash->plist
                    (au:merge-tables (au:plist->hash args) old-args))))
    (setf (au:href (components-table node) type id)
          (list :merge-id id :policy policy :args new-args))))

(defun make-component-table (prefab)
  (au:do-hash-values (node (parse-tree prefab))
    (let* ((parent (parent node))
           (path-policy (or (getf (options node) :policy)
                            (and parent (getf (options parent) :policy)))))
      (dolist (component (components node))
        (destructuring-bind (type (&key merge-id policy) . args) component
          (unless (au:href (components-table node) type)
            (setf (au:href (components-table node) type) (au:dict #'equalp)))
          (merge-component (or policy path-policy) node type merge-id args))))))
