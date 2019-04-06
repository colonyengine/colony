(in-package :fl.prefab)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%library :reader library
             :initarg :library)
   (%data :reader data
          :initarg :data)
   (%parse-tree :reader parse-tree
                :initform (au:dict #'equalp))
   (%root :reader root)
   (%links :reader links
           :initform (au:dict #'eq
                              :source->targets (au:dict #'equalp)
                              :target->source (au:dict #'equalp)))
   (%func :reader func
          :initform (constantly nil))))

(au:define-printer (prefab stream :type t)
  (format stream "~a" (name prefab)))

(defclass node ()
  ((%name :reader name
          :initarg :name)
   (%id :reader id
        :initarg :id
        :initform nil)
   (%display-id :reader display-id
                :initarg :display-id
                :initform nil)
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
                      :initform (au:dict #'eq))
   (%parent :reader parent
            :initarg :parent
            :initform nil)
   (%children :reader children
              :initform (au:dict #'equalp))))

(au:define-printer (node stream :type t)
  (format stream "~a" (path node)))

(au:eval-always
  (defun split-spec (spec)
    (destructuring-bind (name &rest body) spec
      (loop :for tail :on body
            :for item = (first tail)
            :while (symbolp (first item))
            :collect item :into components
            :finally (return (values name components tail))))))

(defun explode-path (path)
  (au:string-split path #\/))

(defun make-node-path (parent name)
  (au:string-merge parent "/" name))

(defun make-node-path-from-parts (path-parts)
  (format nil "/~{~a~^/~}" path-parts))

(defun find-library (name)
  (au:if-found (library (au:href (fl.data:get 'prefabs) name))
               library
               (error "Prefab library ~s does not exist." name)))

(defun %find-prefab (name library)
  (let ((library (find-library library)))
    (au:href library name)))

(defun find-prefab (name library)
  (or (%find-prefab name library)
      (error "Prefab ~s not found in library ~s." name library)))

(defun %find-node (path library)
  (let* ((name (first (explode-path path)))
         (prefab (%find-prefab name library)))
    (when prefab
      (au:href (parse-tree prefab) path))))

(defun find-node (path library)
  (or (%find-node path library)
      (error "Prefab node ~s not found in library ~s." path library)))

(defun map-nodes (func node)
  (funcall func node)
  (au:do-hash-values (child (children node))
    (map-nodes func child)))

(defun parse-copy/link (library id display-id path copy-p link-p form)
  (flet ((check-source (source)
           (ensure-copy/link-source-valid path source)
           (ensure-copy/link-source-absolute path source)
           (ensure-copy/link-source-no-trailing-slash path source))
         (make-path-options (source from)
           (values path
                   id
                   display-id
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
           (direct-path (path &key id display-id)
             (check-path path)
             (values (make-node-path parent path)
                     id
                     display-id
                     (list :mode 'direct))))
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
                                  (link nil link-p))
               options
             (ensure-path-options-keys path options)
             (au:if-let ((copy/link-form (or copy link)))
               (parse-copy/link
                library id display-id path copy-p link-p copy/link-form)
               (direct-path target :id id :display-id display-id)))))))))

(defun make-node (prefab path)
  (symbol-macrolet ((node (au:href (parse-tree prefab) path)))
    (unless node
      (let ((name (first (last (explode-path path)))))
        (setf node (make-instance 'node
                                  :name name
                                  :prefab prefab
                                  :path path
                                  :options '(:mode direct)))))
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
                       (make-node prefab path)
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
    (dolist (component (components node))
      (destructuring-bind (type (&key merge-id policy) . args) component
        (unless (au:href (components-table node) type)
          (setf (au:href (components-table node) type) (au:dict #'equalp)))
        (merge-component policy node type merge-id args)))))

(defun print-prefab (name library)
  (flet ((print-line (level value)
           (format t "~&~a~v@<~s~>~%"
                   (make-string level :initial-element #\Space)
                   level value)))
    (map-nodes
     (lambda (x)
       (let ((level (* 3 (1- (length (explode-path (path x)))))))
         (print-line level (name x))
         (au:do-hash (type table (components-table x))
           (au:do-hash-values (data table)
             (print-line level `(,type ,@(getf data :args)))))))
     (root (find-prefab name library)))))

(defun parse-prefab (prefab)
  (let (success-p)
    (with-slots (%name %library %data) prefab
      (unwind-protect
           (progn
             (make-parse-tree prefab %data)
             (verify-components prefab)
             (insert-missing-transforms prefab)
             (copy-source-nodes prefab)
             (make-component-table prefab)
             (make-relationships prefab)
             (update-links prefab)
             (setf success-p t))
        (unless success-p
          (remhash %name (au:href (fl.data:get 'prefabs) %library)))))))

(defun make-prefab (name library data)
  (let ((prefab (or (%find-prefab name library)
                    (make-instance 'prefab :name name :library library))))
    (with-slots (%data %parse-tree) prefab
      (setf %data data)
      (clrhash %parse-tree))
    prefab))

(defun make-actors (context prefab)
  (let ((actors (au:dict #'equalp)))
    (au:do-hash (path node (parse-tree prefab))
      (with-slots (%name %id %display-id) node
        (setf (au:href actors path)
              (make-actor context
                          :prefab-node node
                          :id %id
                          :display-id (or %display-id %name)))))
    actors))

(defun make-actor-components (context actors)
  (let (components)
    (au:do-hash-values (actor actors)
      (au:do-hash (type table (au:href (components-table (prefab-node actor))))
        (au:do-hash-values (data table)
          (let ((component (make-component context type)))
            (attach-component actor component)
            (push (list data actor component) components)))))
    (dolist (c components)
      (destructuring-bind (data actor component) c
        (let ((args (loop :for (k v) :on (getf data :args) :by #'cddr
                          :append (list k (funcall v context)))))
          (apply #'reinitialize-instance component :actor actor args))))))

(defun make-actor-relationships (context prefab actors &optional parent)
  (let ((parent (or parent (scene-tree (core context))))
        (root (au:href actors (path (root prefab)))))
    (au:do-hash-values (actor actors)
      (let ((node (prefab-node actor)))
        (au:do-hash-values (child (children node))
          (fl.comp:transform-add-child
           (actor-component-by-type actor 'fl.comp:transform)
           (actor-component-by-type (au:href actors (path child))
                                    'fl.comp:transform)))))
    (fl.comp:transform-add-child
     (actor-component-by-type parent 'fl.comp:transform)
     (actor-component-by-type root 'fl.comp:transform))))

(defun make-factory (prefab setter)
  (lambda (core)
    (let* ((context (context core))
           (actors (make-actors context prefab)))
      (funcall setter actors)
      (make-actor-components context actors)
      (make-actor-relationships context prefab actors)
      (au:do-hash-values (actor actors)
        (spawn-actor actor)))))

(defun load-prefabs (core prefabs)
  (%fl:make-scene-tree core)
  (dolist (spec prefabs)
    (destructuring-bind (name library) spec
      (let ((prefab (find-prefab name library)))
        (funcall (func prefab) core)))))

(defun thunk-component-args (context data)
  (destructuring-bind (type . options/args) data
    (let ((options-p (listp (first options/args))))
      `(list ',type
             ',(when options-p (first options/args))
             ,@(loop :with args = (if options-p
                                      (rest options/args)
                                      options/args)
                     :for (key value) :on args :by #'cddr
                     :collect key
                     :collect `(lambda (,context)
                                 (declare (ignorable ,context))
                                 ,value))))))

(defmacro thunk-spec (context spec)
  (labels ((traverse-children (data)
             (au:mvlet ((name components children (split-spec data)))
               `(list ',name
                      ,@(mapcar
                         (lambda (x) (thunk-component-args context x))
                         components)
                      ,@(mapcar #'traverse-children children)))))
    `(list ,@(mapcar #'traverse-children spec))))

(defmacro preprocess-spec (context spec)
  (au:with-unique-names (setter actors)
    `(let ((,actors (au:dict)))
       (flet ((,setter (arg)
                (setf ,actors arg))
              (ref (actor)
                (let ((actor (au:href ,actors actor)))
                  (values actor ,actors))))
         (ref nil)
         (values
          (thunk-spec ,context ,spec)
          #',setter)))))

(defmacro define-prefab (name (&key library (context 'context)) &body body)
  (let* ((libraries '(fl.data:get 'prefabs))
         (prefabs `(au:href ,libraries ',library))
         (body (list (cons name body))))
    (au:with-unique-names (prefab data setter)
      `(progn
         (ensure-prefab-name-string ',name)
         (ensure-prefab-name-valid ',name)
         (ensure-prefab-library-set ',name ',library)
         (ensure-prefab-library-symbol ',name ',library)
         (unless ,libraries
           (fl.data:set 'prefabs (au:dict #'eq)))
         (unless ,prefabs
           (setf ,prefabs (au:dict #'equalp)))
         (au:mvlet* ((,data ,setter (preprocess-spec ,context ,body))
                     (,prefab (make-prefab ',name ',library ,data)))
           (setf (au:href ,prefabs ',name) ,prefab
                 (slot-value ,prefab '%func) (make-factory ,prefab ,setter))
           (parse-prefab ,prefab))
         (export ',library)))))
