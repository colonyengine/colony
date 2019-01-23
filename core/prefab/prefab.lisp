(in-package :fl.prefab)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%library :reader library
             :initarg :library)
   (%paths :reader paths
           :initform (u:dict #'equalp))
   (%root :reader root)
   (%source->targets :reader source->targets
                     :initform (u:dict #'equalp))
   (%target->source :reader target->source
                    :initform (u:dict #'equalp))))

(u:define-printer (prefab stream :type t)
  (format stream "~a" (name prefab)))

(defclass node ()
  ((%name :reader name
          :initarg :name)
   (%prefab :reader prefab
            :initarg :prefab)
   (%data :reader data
          :initarg :data)
   (%source-data :reader source-data
                 :initform nil)
   (%path :reader path
          :initarg :path)
   (%components :reader components
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
  (fl.util:split-sequence #\/ path :remove-empty-subseqs t))

(defun make-node-path (parent name)
  (concatenate 'string (when parent (path parent)) "/" name))

(defun map-nodes (func node)
  (funcall func node)
  (u:do-hash-values (child (children node))
    (map-nodes func child)))

(defun update-prefab-paths (prefab)
  (with-slots (%paths %root) prefab
    (clrhash %paths)
    (map-nodes
     (lambda (x)
       (setf (u:href %paths (path x)) x))
     %root)))

(defun make-node (name data &key prefab parent)
  (let* ((prefab (or prefab (prefab parent)))
         (path (make-node-path parent name))
         (node (or (%find-node path (library prefab))
                   (make-instance 'node
                                  :prefab prefab
                                  :name name
                                  :path path
                                  :data data
                                  :parent parent))))
    (setf (u:href (paths prefab) path) node)
    node))

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
      (u:href (paths prefab) path))))

(defun find-node (path library)
  (or (%find-node path library)
      (error "Prefab node ~s not found in library ~s." path library)))

(defun ensure-component-valid (node type args)
  (with-slots (%path %components) node
    (unless (get-computed-component-precedence-list type)
      (error "Component type does not exist: ~s.~%Prefab path: ~s."
             type %path))
    (when (oddp (length args))
      (error "Component type has an odd number of arguments: ~s.~%Prefab path: ~s."
             type %path))
    (loop :with valid-args = (compute-component-initargs type)
          :for (key value) :on args :by #'cddr
          :unless (member key valid-args)
            :do (error "Invalid argument: ~s.~%Component type: ~s.~%Prefab path: ~s."
                       key type %path))
    (when (u:href %components 'fl.comp:transform)
      (error "Cannot have multiple transform components per node.~%Prefab path: ~s."
             %path))))

(defun ensure-component-policies-valid (path type policies)
  (when (every (u:rcurry #'find policies) '(prefer-new prefer-old))
    (error "Component policies must not have both PREFER-NEW and PREFER-OLD.~%~
            Component type: ~s~%Prefab path: ~s."
           type path)))

(defun parse-component-spec (node component-spec)
  (with-slots (%path %components) node
    (destructuring-bind (type (&key (id 0) (policies '(new-type))) . args) component-spec
      (ensure-component-valid node type args)
      (ensure-component-policies-valid %path type policies)
      (unless (integerp id)
        (error "Component type ~s must have an integer ID, but ~s is of type: ~s.~%Prefab path: ~s."
               type id (type-of id) %path))
      (unless (u:href %components type)
        (setf (u:href %components type) (u:dict #'eql)))
      (dolist (policy policies)
        (resolve-component-conflicts policy node type id args)))))

(defgeneric resolve-component-conflicts (policy node type id args))

(defmethod resolve-component-conflicts ((policy (eql 'new-type)) node type id args)
  (setf (u:href (components node) type id) args))

(defmethod resolve-component-conflicts ((policy (eql 'old-type)) node type id args)
  (u:unless-found (component (u:href (components node) type id))
    (setf (u:href (components node) type id) args)))

(defmethod resolve-component-conflicts ((policy (eql 'new-args)) node type id args)
  (let* ((old-args (u:plist->hash (u:href (components node) type id)))
         (new-args (u:hash->plist (u:merge-tables old-args (u:plist->hash args)))))
    (setf (u:href (components node) type id) new-args)))

(defmethod resolve-component-conflicts ((policy (eql 'old-args)) node type id args)
  (let* ((old-args (u:plist->hash (u:href (components node) type id)))
         (new-args (u:hash->plist (u:merge-tables (u:plist->hash args) old-args))))
    (setf (u:href (components node) type id) new-args)))

(defun parse-components (node data)
  (with-slots (%components) node
    (setf %components (u:dict #'eq))
    (dolist (component-spec data)
      (parse-component-spec node component-spec))
    (unless (u:href %components 'fl.comp:transform)
      (setf (u:href %components 'fl.comp:transform) (u:dict #'eql 0 nil)))))

(defun parse-copy/link (node target copy-p link-p form)
  (with-slots (%prefab %path) node
    (destructuring-bind (source &key (from (library %prefab))) form
      (unless (stringp source)
        (error "The source of a :LINK or :COPY must be a string.~%Prefab path: ~s."
               %path))
      (unless (char= (elt source 0) #\/)
        (error "The source of :LINK or :COPY must be an absolute path.~%Prefab path: ~s."
               %path))
      (unless (symbolp from)
        (error ":FROM must be a symbol, but ~s is of type: ~s.~%Prefab path: ~s."
               from (type-of from) %path))
      (list (cond (copy-p 'copy) (link-p 'link))
            target
            :source source
            :from from
            :to (library %prefab)))))

(defun parse-path-spec (node path-spec)
  (with-slots (%prefab %path) node
    (flet ((new-node (target)
             (list 'new target :to (library %prefab))))
      (typecase path-spec
        (string
         (new-node path-spec))
        (list
         (destructuring-bind (target (&key (copy nil copy-p) (link nil link-p))) path-spec
           (when (and copy-p link-p)
             (error "Only one of :COPY or :LINK can be specified for a path.~%Prefab path: ~s."
                    %path))
           (u:if-let ((copy/link-form (or copy link)))
             (parse-copy/link node target copy-p link-p copy/link-form)
             (new-node target))))
        (t (error "A path must be a string or a list of a string and options.~%Prefab path: ~s"
                  %path))))))

(defun parse-children (parent data)
  (labels ((check (name parent)
             (unless (stringp name)
               (error "Node name must be a string, but ~s is of type: ~s.~%Prefab path: ~s."
                      name (type-of name) (path parent)))
             (when (char= (elt name 0) #\/)
               (error "Target path ~s must be relative, not absolute.~%Prefab path: ~s."
                      name (path parent))))
           (parse-child (parent data)
             (destructuring-bind (path-spec . body) data
               (let ((child-options (parse-path-spec parent path-spec)))
                 (destructuring-bind (mode target &rest args &key &allow-other-keys) child-options
                   (check target parent)
                   (apply #'add-child mode target parent body args))))))
    (dolist (x data)
      (let ((child (parse-child parent x)))
        (setf (u:href (children parent) (name child)) child)))))

(defun parse-node (node)
  (with-slots (%data %source-data) node
    (u:mvlet ((components children (split-components/children %data))
              (source-components source-children (split-components/children %source-data)))
      (parse-components node (append source-components components))
      (parse-children node (append source-children children)))))

(defun make-prefab (name library data)
  (unless (stringp name)
    (error "Prefab name must be a string, but ~s is of type ~s."
           name (type-of name)))
  (when (find #\/ name)
    (error "Prefab name must not contain a \"/\" character.~%Prefab: ~s." name))
  (let ((prefab (or (%find-prefab name library)
                    (make-instance 'prefab :name name :library library))))
    (with-slots (%paths %root) prefab
      (clrhash %paths)
      (setf %root (make-node name data :prefab prefab)))
    prefab))

(defun remove-broken-links (prefab)
  (with-slots (%library %source->targets %target->source) prefab
    (u:do-hash (target source %target->source)
      (destructuring-bind (target-library . target-path) target
        (unless (%find-node target-path target-library)
          (remhash target %target->source)
          (v:warn :fl.core.prefab
                  "Removed the link from ~s to ~s because the target no longer exists."
                  source target-path)
          (remhash target (u:href %source->targets source))
          (unless (u:href %source->targets source)
            (remhash source %source->targets)))))
    (u:do-hash-keys (source %source->targets)
      (unless (%find-node source %library)
        (remhash source %source->targets)
        (v:warn :fl.core.prefab
                "Remove all links from ~s because the source no longer exists."
                source)
        (u:do-hash (k v %target->source)
          (when (string= v source)
            (remhash k %target->source)))))))

(defun update-links (prefab)
  (with-slots (%source->targets) prefab
    (remove-broken-links prefab)
    (u:do-hash (source targets %source->targets)
      (let ((source-node (find-node source (library prefab))))
        (u:do-hash-values (target targets)
          (setf (slot-value target '%source-data) (data source-node))
          (parse-node target)
          (update-prefab-paths (prefab target)))))))

(defun make-link (source target)
  (let* ((target-key (cons (library (prefab target)) (path target))))
    (with-slots (%prefab %path) source
      (symbol-macrolet ((targets (u:href (source->targets %prefab) %path)))
        (setf (u:href (target->source %prefab) target-key) %path)
        (unless targets
          (setf targets (u:dict #'equalp)))
        (setf (u:href targets target-key) target))
      (remove-broken-links %prefab))))

(defgeneric add-child (mode target parent data &key &allow-other-keys))

(defmethod add-child ((mode (eql 'new)) target parent data &key)
  (let* ((path-parts (explode-path target))
         (name (first path-parts))
         (new-data (rest
                    (reduce #'list
                            (butlast path-parts)
                            :initial-value (cons (car (last path-parts)) data)
                            :from-end t)))
         (child (make-node name new-data :parent parent)))
    (parse-node child)
    child))

(defmethod add-child ((mode (eql 'copy)) target parent data &key from to source)
  (unless (stringp source)
    (error "~s requires a string path, but ~s is of type: ~s.~%Prefab: ~s."
           mode source (type-of source) (name (prefab parent))))
  (let* ((source-data (data (find-node source from)))
         (child (add-child 'new target parent data))
         (target-node (find-node (make-node-path parent target) to)))
    (setf (slot-value target-node '%source-data) source-data)
    (parse-node target-node)
    child))

(defmethod add-child ((mode (eql 'link)) target parent data &rest args &key from to source)
  (let* ((child (apply #'add-child 'copy target parent data args))
         (source-node (find-node source from))
         (target-node (find-node (make-node-path parent target) to)))
    (make-link source-node target-node)
    child))

(defmacro define-prefab (name (&optional library) &body body)
  (let* ((libraries '(fl.data:get 'prefabs))
         (prefabs `(u:href ,libraries ',library)))
    (unless library
      (error "Prefab ~s must have a library." name))
    (unless (and (symbolp library)
                 (not (keywordp library)))
      (error "Prefab library must be a non-keyword symbol, but ~s is of type ~s.~% Prefab: ~s."
             library (type-of library) name))
    (u:with-unique-names (prefab)
      `(progn
         (unless ,libraries
           (fl.data:set 'prefabs (u:dict #'eq)))
         (unless ,prefabs
           (setf ,prefabs (u:dict #'equalp)))
         (let ((,prefab (make-prefab ',name ',library ',body)))
           (setf (u:href ,prefabs ',name) ,prefab)
           (parse-node (root ,prefab))
           (update-links ,prefab))
         (export ',library)))))

(defun print-prefab (name library)
  (flet ((print-line (level value)
           (format t "~a~v@<~s~>~%"
                   (make-string level :initial-element #\Space)
                   level value)))
    (map-nodes
     (lambda (x)
       (let ((level (* 3 (1- (length (explode-path (path x)))))))
         (print-line level (name x))
         (u:do-hash (type table (components x))
           (u:do-hash-values (args table)
             (print-line level `(,type ,@args))))
         (format t "~%")))
     (root (find-prefab name library)))))

(define-prefab "foo" (test2)
  ("bar"
   (fl.comp:camera () :active-p t :zoom 5)
   ("baz"
    ("qux"))))

(define-prefab "table" (test)
  (fl.comp:mesh () :location '(:mesh "table.glb"))
  (fl.comp:render () :material 'table)
  ("glass/a/b"
   (fl.comp:mesh () :location '(:mesh "cube.glb"))
   (fl.comp:render () :material 'glass)
   (("place/mat" (:link ("/foo/bar" :from test2)))
    (fl.comp:camera (:policies (new-args)) :zoom 10)
    ("test1"
     ("test2")))))
