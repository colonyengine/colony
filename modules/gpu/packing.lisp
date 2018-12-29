(in-package :first-light.gpu)

(defun pack-container (type)
  (etypecase type
    (varjo:v-vector :vec)
    (varjo:v-matrix :mat)
    (varjo:v-array :array)))

(defun pack-type (type)
  (etypecase type
    (varjo:v-bool '(:bool))
    (varjo:v-int '(:int 32))
    (varjo:v-uint '(:uint 32))
    (varjo:v-float '(:float 32))
    (varjo:v-struct (varjo:type->type-spec type))
    (varjo:v-container
     (let ((element-type (varjo:v-element-type type)))
       (if (or (typep element-type 'varjo:v-user-struct)
               (typep element-type 'varjo:v-array))
           (error "Shader blocks containing arrays of aggregates are not currently supported.")
           (list* (pack-container type)
                  (pack-type element-type)
                  (varjo:v-dimensions type)))))))

(defun pack-struct (struct)
  (loop :with name = (varjo:type->type-spec struct)
        :for (slot-name slot-type) :in (varjo.internals:v-slots struct)
        :for type = (pack-type slot-type)
        :collect (list slot-name type) :into members
        :finally (return `(,name (:struct () ,@members)))))

(defun pack-block (layout)
  (loop :with uniform = (uniform layout)
        :with name = (varjo:name uniform)
        :with layout-type = (layout-type layout)
        :for (slot-name slot-type) :in (varjo.internals:v-slots (varjo:v-type-of uniform))
        :for packed-type = (pack-type slot-type)
        :collect (list slot-name packed-type) :into members
        :finally (return `(,name (:block (:packing ,layout-type) ,@members)))))

(defun pack-layout (layout)
  (let ((structs (collect-layout-structs layout)))
    (glsl-packing:pack-structs
     `(,@(mapcar #'pack-struct structs)
       ,(pack-block layout)))))

(defun unpack-type (layout-type type)
  (destructuring-bind ((spec &optional x y z) &key &allow-other-keys) type
    (labels ((get-container (x)
               (unpack-type layout-type (list x)))
             (get-stride (count)
               (ecase layout-type
                 (:std140 4)
                 (:std430 (if (= count 3) 4 count))))
             (get-result (&rest args)
               (destructuring-bind (&optional (count 1) rows) (getf args :dimensions)
                 (list :dimensions (cons count rows)
                       :element-type (getf args :element-type)
                       :element-stride (get-stride count)))))
      (ecase spec
        ((:bool :uint) (get-result :element-type '(unsigned-byte 32)))
        (:int (get-result :element-type '(signed-byte 32)))
        (:float (get-result :element-type 'single-float))
        ((:vec :mat) (apply #'get-result :dimensions (list y z) (get-container x)))
        (:array (get-container x))))))
