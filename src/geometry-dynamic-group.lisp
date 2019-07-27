(in-package #:%first-light)

(defclass geometry-group ()
  ((%name :reader name
          :initarg :name)
   (%format :reader buffer-format
            :initarg :format)
   (%divisor :reader divisor
             :initarg :divisor)
   (%attributes :reader attributes
                :initarg :attributes)
   (%attribute-order :reader attribute-order
                     :initarg :attribute-order)))

(defclass geometry-group/separate (geometry-group) ())

(defclass geometry-group/interleaved (geometry-group) ())

(defun make-geometry-groups (spec)
  (let ((groups (u:dict))
        (order))
    (dolist (group spec)
      (destructuring-bind (name (&key (format 'interleaved) (divisor 0))
                           . attrs)
          group
        (u:mvlet ((group-type (a:format-symbol :%first-light
                                               "GEOMETRY-GROUP/~a"
                                               format))
                  (attributes attribute-order (make-dynamic-attributes attrs)))
          (push name order)
          (setf (u:href groups name)
                (make-instance
                 group-type
                 :name name
                 :format format
                 :divisor divisor
                 :attributes attributes
                 :attribute-order attribute-order)))))
    (values groups
            (nreverse order))))

(defgeneric get-geometry-group-buffer-count (group)
  (:method (group)
    1)
  (:method ((group geometry-group/separate))
    (hash-table-count (attributes group))))

(defun get-geometry-group-attribute-size (group)
  (reduce #'+ (u:hash-values (attributes group))
          :key #'get-geometry-attribute-size))

(defmethod configure-geometry-group ((group geometry-group) index buffers)
  (with-slots (%attributes %attribute-order %divisor) group
    (loop :for attr-name :in %attribute-order
          :for attr = (u:href %attributes attr-name)
          :for i :from index
          :for buffer :across buffers
          :do (gl:bind-buffer :array-buffer buffer)
              (configure-dynamic-attribute attr i 0 0 %divisor))))

(defmethod configure-geometry-group ((group geometry-group/interleaved) index
                                     buffers)
  (with-slots (%attributes %attribute-order %divisor) group
    (gl:bind-buffer :array-buffer (aref buffers 0))
    (loop :with stride = (get-geometry-group-attribute-size group)
          :with offset = 0
          :for attr-name :in %attribute-order
          :for attr = (u:href %attributes attr-name)
          :for i :from index
          :do (configure-dynamic-attribute attr i stride offset %divisor)
              (incf offset (get-geometry-attribute-size attr)))))
