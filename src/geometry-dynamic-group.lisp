(in-package #:virality.geometry)

(defclass group ()
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

(defclass group/separate (group) ())

(defclass group/interleaved (group) ())

(defun make-groups (spec)
  (let ((groups (u:dict))
        (order))
    (dolist (group spec)
      (destructuring-bind (name (&key (format 'interleaved) (divisor 0))
                           . attrs)
          group
        (u:mvlet ((group-type (a:format-symbol
                               :virality.geometry "GROUP/~a" format))
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

(defgeneric get-group-buffer-count (group)
  (:method (group)
    1)
  (:method ((group group/separate))
    (hash-table-count (attributes group))))

(defun get-group-attribute-size (group)
  (reduce #'+ (u:hash-values (attributes group))
          :key #'get-attribute-size))

(defmethod configure-group ((group group) index buffers)
  (with-slots (%attributes %attribute-order %divisor) group
    (loop :for attr-name :in %attribute-order
          :for attr = (u:href %attributes attr-name)
          :for i :from index
          :for buffer :across buffers
          :do (gl:bind-buffer :array-buffer buffer)
              (configure-dynamic-attribute attr i 0 0 %divisor))))

(defmethod configure-group ((group group/interleaved) index buffers)
  (with-slots (%attributes %attribute-order %divisor) group
    (gl:bind-buffer :array-buffer (aref buffers 0))
    (loop :with stride = (get-group-attribute-size group)
          :with offset = 0
          :for attr-name :in %attribute-order
          :for attr = (u:href %attributes attr-name)
          :for i :from index
          :do (configure-dynamic-attribute attr i stride offset %divisor)
              (incf offset (get-attribute-size attr)))))
