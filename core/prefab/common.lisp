(in-package #:first-light.prefab)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%library :reader library
             :initarg :library)
   (%doc :reader doc
         :initarg :doc)
   (%data :reader data
          :initarg :data)
   (%parse-tree :reader parse-tree
                :initform (u:dict #'equalp))
   (%root :reader root)
   (%links :reader links
           :initform (u:dict :source->targets (u:dict #'equalp)
                             :target->source (u:dict #'equalp)))
   (%func :accessor func
          :initform (constantly nil))))

(u:define-printer (prefab stream :type t)
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
                      :initform (u:dict))
   (%parent :reader parent
            :initarg :parent
            :initform nil)
   (%children :reader children
              :initform (u:dict #'equalp))))

(u:define-printer (node stream :type t)
  (format stream "~a" (path node)))

(u:eval-always
  ;; Each component initialization argument is converted temporarily to an
  ;; instance of this class which, after we figure out which argument values
  ;; are actually valid and present due to component merge policies, we then
  ;; initialize the env and force the thunk. This is usef to implement the
  ;; FL:REF function in the value form of the components.
  (defclass injectable-ref-value-thunk ()
    (;; A lambda function wrapped around the value lexically supplying a CONTEXT
     ;; variable. This function also exists in a injection ref environment (a
     ;; closure), which means the REF function is available in the value of the
     ;; argument.
     (%thunk :reader thunk
             :initarg :thunk)
     ;; Each injection ref environment has a secret back door to fill in the
     ;; lexical variables that the FL:REF lexicaly scoped call needs to process
     ;; each argument.  We use this to poke in the values to the lexical closure
     ;; before evaluating the thunk.
     (%env-injection-control-func :reader env-injection-control-func
                                  :initarg :env-injection-control-func
                                  :initform (constantly nil))))

  (defun make-injectable-ref-value-thunk (&rest init-args)
    (apply #'make-instance 'injectable-ref-value-thunk init-args)))

(u:eval-always
  (defun split-spec (spec)
    (destructuring-bind (name &rest body) spec
      (loop :for tail :on body
            :for item = (first tail)
            :while (symbolp (first item))
            :collect item :into components
            :finally (return (values name components tail))))))

(defun explode-path (path)
  (split-sequence:split-sequence #\/ path :remove-empty-subseqs t))

(defun make-node-path (parent name)
  (let ((path (u:string-merge parent "/" name)))
    (string-right-trim "/" path)))

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
  (let* ((name (first (explode-path path)))
         (prefab (%find-prefab name library)))
    (when prefab
      (u:href (parse-tree prefab) path))))

(defun find-node (path library)
  (or (%find-node path library)
      (error "Prefab node ~s not found in library ~s." path library)))

(defun map-nodes (func node)
  (funcall func node)
  (u:do-hash-values (child (children node))
    (map-nodes func child)))

(defun make-prefab (name library doc data)
  (let ((prefab (or (%find-prefab name library)
                    (make-instance 'prefab :name name
                                           :library library
                                           :doc doc))))
    (with-slots (%data %parse-tree) prefab
      (setf %data data)
      (clrhash %parse-tree))
    prefab))

(defun get-node-option (node option &optional (inherit-p t))
  (let ((parent (parent node)))
    (or (getf (options node) option)
        (and inherit-p
             parent
             (getf (options parent) option)))))
