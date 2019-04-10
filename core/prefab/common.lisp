(in-package :first-light.prefab)

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
                :initform (au:dict #'equalp))
   (%root :reader root)
   (%links :reader links
           :initform (au:dict #'eq
                              :source->targets (au:dict #'equalp)
                              :target->source (au:dict #'equalp)))
   (%func :accessor func
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
  (let ((path (au:string-merge parent "/" name)))
    (string-right-trim "/" path)))

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
