(in-package :first-light.prefab)

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

(defun make-prefab (name library data)
  (let ((prefab (or (%find-prefab name library)
                    (make-instance 'prefab :name name :library library))))
    (with-slots (%data %parse-tree) prefab
      (setf %data data)
      (clrhash %parse-tree))
    prefab))

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