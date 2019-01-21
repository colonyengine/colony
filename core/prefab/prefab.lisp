(in-package :fl.prefab)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%library :reader library
             :initarg :library)
   (%paths :reader paths
           :initform (u:dict #'equalp))
   (%root :reader root)
   (source->targets :reader source->targets
                    :initform (u:dict #'equalp))
   (target->source :reader target->source
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

(defun make-node (name data &key prefab parent)
  (let* ((prefab (or prefab (prefab parent)))
         (path (make-node-path parent name))
         (node (make-instance 'node
                              :prefab prefab
                              :name name
                              :path path
                              :data data
                              :parent parent)))
    (when (u:href (paths prefab) path)
      (error "Node path ~s already exists.~%Prefab: ~s.~%Library: ~s."
             path (name prefab) (library prefab)))
    (setf (u:href (paths prefab) path) node)))

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

(defun find-node (path library)
  (let* ((prefab-name (first (explode-path path)))
         (prefab (%find-prefab prefab-name library)))
    (u:href (paths prefab) path)))

(defun parse-components (node data)
  (flet ((check (type args path components)
           (unless (get-computed-component-precedence-list type)
             (error "Component type does not exist: ~s.~%Prefab path: ~s."
                    type path))
           (when (oddp (length args))
             (error "Component type has an odd number of arguments: ~s.~%Prefab path: ~s."
                    type path))
           (loop :with valid-args = (compute-component-initargs type)
                 :for (key value) :on args :by #'cddr
                 :unless (member key valid-args)
                   :do (error "Invalid argument: ~s.~%Component type: ~s.~%Prefab path: ~s."
                              key type path))
           (when (u:href components 'fl.comp:transform)
             (error "Cannot have multiple transform components per node.~%Prefab path: ~s."
                    path))))
    (let ((table (u:dict #'eq)))
      (dolist (x data)
        (destructuring-bind (type . args) (u:ensure-list x)
          (check type args (path node) table)
          (push args (u:href table type))))
      (unless (u:href table 'fl.comp:transform)
        (push nil (u:href table 'fl.comp:transform)))
      table)))

(defun parse-children (parent data)
  (labels ((check (name parent)
             (unless (stringp name)
               (error "Node name must be a string, but ~s is of type: ~s.~%Prefab path: ~s."
                      name (type-of name) (path parent)))
             (when (char= (elt name 0) #\/)
               (error "Target path ~s must be relative, not absolute.~%Prefab path: ~s."
                      name (path parent))))
           (parse-child (parent data)
             (destructuring-bind (id . body) data
               (let ((id (if (stringp id) (list :new nil :as id) id))
                     (to (library (prefab parent))))
                 (destructuring-bind (mode source &key (from to) as) id
                   (check as parent)
                   (add-child mode parent body :from from :to to :source source :target as))))))
    (dolist (x data)
      (u:when-let ((child (parse-child parent x)))
        (setf (u:href (children parent) (name child)) child)))))

(defun parse-node (node)
  (with-slots (%path %data %components) node
    (u:mvlet ((components children (split-components/children %data)))
      (setf %components (parse-components node components))
      (parse-children node children))))

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

(defun make-link (source target)
  (let* ((target-key (cons (library (prefab target)) (path target))))
    (with-slots (%prefab %path) source
      (symbol-macrolet ((targets (u:href (source->targets %prefab) %path)))
        (setf (u:href (target->source %prefab) target-key) %path)
        (unless targets
          (setf targets (u:dict #'equalp)))
        (setf (u:href targets target-key) target)))))

(defgeneric add-child (mode parent data &key &allow-other-keys))

(defmethod add-child ((mode (eql :new)) parent data &key target)
  (let* ((path-parts (explode-path target))
         (name (first path-parts))
         (new-data (rest (reduce #'list
                                 (butlast path-parts)
                                 :initial-value (cons (car (last path-parts)) data)
                                 :from-end t)))
         (child (make-node name new-data :parent parent)))
    (parse-node child)))

(defmethod add-child ((mode (eql :copy)) parent data &key from source target)
  (unless (stringp source)
    (error "~s requires a string path, but ~s is of type: ~s.~%Prefab: ~s."
           mode source (type-of source) (name (prefab parent))))
  (let ((source-data (data (find-node source from))))
    (add-child :new parent source-data :target target)))

(defmethod add-child ((mode (eql :link)) parent data &key from to source target)
  (let* ((source-node (find-node source from))
         (child (add-child :new parent (data source-node) :target target))
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
           (parse-node (root ,prefab)))
         (export ',library)))))

(define-prefab "foo" (test)
  ("bar"
   ("baz"
    ("qux"))))

(define-prefab "table" (test)
  (fl.comp:mesh :location '(:mesh "table.glb"))
  (fl.comp:render :material 'table)
  ("glass/a/b"
   (fl.comp:mesh :location '(:mesh "cube.glb"))
   (fl.comp:render :material 'glass)
   ((:link "/foo/bar" :as "place/mat")
    (fl.comp:transform :scale (fl.math:vec3 1)))))
