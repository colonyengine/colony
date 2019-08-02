(in-package #:virality.engine)

(defclass dynamic-attribute ()
  ((%name :reader name
          :initarg :name)
   (%normalize-p :reader normalize-p
                 :initarg :normalize-p)
   (%type :reader attr-type
          :Initarg :type)
   (%out-type :reader out-type
              :initarg :out-type)
   (%count :reader element-count
           :initarg :count)))

(defun make-dynamic-attributes (spec)
  (let ((attrs (u:dict))
        (order))
    (dolist (attribute spec)
      (destructuring-bind (name &key normalize (type :float) (out-type type)
                                  (count 1))
          attribute
        (push name order)
        (setf (u:href attrs name)
              (make-instance 'dynamic-attribute
                             :name name
                             :normalize-p normalize
                             :type (a:make-keyword type)
                             :out-type (a:make-keyword out-type)
                             :count count))))
    (values attrs
            (nreverse order))))

(defun get-geometry-attribute-size (attribute)
  (with-slots (%type %count) attribute
    (* %count
       (ecase %type
         ((:byte :unsigned-byte) 1)
         ((:short :unsigned-short :half-float) 2)
         ((:int :unsigned-int :float :fixed) 4)
         (:double 8)))))

(defun configure-dynamic-attribute (attribute index stride offset divisor)
  (with-slots (%type %out-type %count %normalize-p) attribute
    (let ((normalize-p (if %normalize-p 1 0)))
      (ecase %out-type
        ((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int)
         (%gl:vertex-attrib-ipointer index %count %type stride offset))
        ((:half-float :float :fixed)
         (%gl:vertex-attrib-pointer
          index %count %type normalize-p stride offset))
        (:double
         (%gl:vertex-attrib-lpointer index %count %type stride offset)))
      (%gl:vertex-attrib-divisor index divisor))))
