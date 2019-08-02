(in-package #:virality.gpu)

(defclass shader-buffer ()
  ((%id :reader id
        :initform (gl:gen-buffer))
   (%name :reader buffer-name
          :initarg :name)
   (%target :reader target
            :initarg :target)
   (%layout :reader layout
            :initarg :layout)))

(defun %make-buffer (name target layout)
  (make-instance 'shader-buffer :name name :target target :layout layout))

(defun buffer-type->target (buffer-type)
  (ecase buffer-type
    (:ubo :uniform-buffer)
    (:ssbo :shader-storage-buffer)))

(defun buffer-type->block-type (type)
  (ecase type
    (:ubo :uniform)
    (:ssbo :buffer)))

(defun block-type->buffer-type (block-type)
  (ecase block-type
    (:uniform :ubo)
    (:buffer :ssbo)))

(defun find-buffer (buffer-name)
  (let ((buffer-table (%fl:meta 'buffers)))
    (u:href buffer-table buffer-name)))

(defun create-buffer (buffer-name block-alias)
  (a:if-let ((block (find-block block-alias)))
    (let* ((buffer-table (%fl:meta 'buffers))
           (type (block-type->buffer-type (block-type block)))
           (target (buffer-type->target type))
           (buffer (%make-buffer buffer-name target (layout block))))
      (with-slots (%id %layout) buffer
        (%gl:bind-buffer target %id)
        (%gl:buffer-data target (size %layout) (cffi:null-pointer) :static-draw)
        (setf (u:href buffer-table buffer-name) buffer)))
    (error "Cannot find the block with alias ~s when attempting to create a ~
            buffer."
           block-alias)))

(defun bind-buffer (buffer-name binding-point)
  (a:if-let ((buffer (find-buffer buffer-name)))
    (with-slots (%target %id) buffer
      (%gl:bind-buffer-base %target binding-point %id)
      (%gl:bind-buffer %target 0))
    (error "Cannot find buffer ~s." buffer-name)))

(defun unbind-buffer (buffer-name)
  (bind-buffer buffer-name 0))

(defun delete-buffer (buffer-name)
  (let ((buffer-table (%fl:meta 'buffers))
        (buffer (find-buffer buffer-name)))
    (unbind-buffer buffer-name)
    (gl:delete-buffers (list (id buffer)))
    (remhash buffer-name buffer-table)))

(defun %write-buffer-member (target member value)
  (with-slots (%element-type %offset %element-stride %byte-stride) member
    (let ((count (length value)))
      (static-vectors:with-static-vector
          (sv (* count %element-stride)
              :element-type %element-type
              :initial-element (coerce 0 %element-type))
        (let ((ptr (static-vectors:static-vector-pointer sv))
              (i 0))
          (map nil
               (lambda (x)
                 (if (typep x 'sequence)
                     (replace sv x :start1 i)
                     (setf (aref sv i) x))
                 (incf i %element-stride))
               value)
          (%gl:buffer-sub-data target %offset (* count %byte-stride) ptr))))))

(defun %write-buffer-member-matrix (target member value)
  (with-slots (%element-type %offset %element-stride %byte-stride %dimensions)
      member
    (let ((count (length value)))
      (destructuring-bind (columns rows) %dimensions
        (static-vectors:with-static-vector
            (sv (* count columns %element-stride)
                :element-type %element-type
                :initial-element (coerce 0 %element-type))
          (let ((ptr (static-vectors:static-vector-pointer sv))
                (i 0))
            (map nil
                 (lambda (x)
                   (loop :repeat columns
                         :for j :from i :by %element-stride
                         :for k :by rows
                         :do (replace sv
                                      x
                                      :start1 j
                                      :start2 k
                                      :end2 (+ k rows)))
                   (incf i (* columns %element-stride)))
                 value)
            (%gl:buffer-sub-data
             target %offset (* count %byte-stride) ptr)))))))

(defun write-buffer-path (buffer-name path value)
  (with-slots (%type %id %target %layout) (find-buffer buffer-name)
    (let ((member (u:href (members %layout) path)))
      (check-type value sequence)
      (gl:bind-buffer %target %id)
      (if (eq (object-type member) :mat)
          (%write-buffer-member-matrix %target member value)
          (%write-buffer-member %target member value))
      (gl:bind-buffer %target 0))))

(defun %read-buffer-member/scalar (member data count)
  (with-slots (%element-stride) member
    (if (= count 1)
        (aref data 0)
        (subseq data 0 (* count %element-stride)))))

(defun %read-buffer-member/vector (member data count)
  (with-slots (%dimensions %element-stride) member
    (let* ((size (car %dimensions))
           (func (a:format-symbol :m "VEC~a" size)))
      (flet ((make-vector (data index size)
               (let ((args (loop :for i :below size
                                 :collect (aref data (+ index i)))))
                 (apply func args))))
        (if (= count 1)
            (make-vector data 0 size)
            (loop :repeat count
                  :for i :by %element-stride
                  :collect (make-vector data i size)))))))

(defun %read-buffer-member/matrix (member data count)
  (with-slots (%dimensions %element-stride) member
    (destructuring-bind (columns rows) %dimensions
      (let ((func (a:format-symbol :m "MAT~d" columns)))
        (flet ((make-matrix (data index)
                 (let ((args (loop :repeat columns
                                   :for i :from index :by %element-stride
                                   :append (loop :for j :below rows
                                                 :collect (aref data (+ i j)))
                                   :do (incf index %element-stride))))
                   (apply func args))))
          (if (or (= columns rows 2)
                  (= columns rows 3)
                  (= columns rows 4))
              (if (= count 1)
                  (make-matrix data 0)
                  (loop :for i :below count
                        :for index = (* columns %element-stride i)
                        :collect (make-matrix data index)))
              (error "Only 2x2, 3x3, and 4x4")))))))

(defun %read-buffer-member (target member &optional count)
  (with-slots (%type %dimensions %count %element-stride %element-type %offset
               %byte-stride)
      member
    (let* ((count (or count %count))
           (element-count (reduce #'* %dimensions))
           (stride (if (and (eq %type :vec)
                            (= element-count 3))
                       4
                       %element-stride))
           (data (make-array (* stride (cadr %dimensions) count)
                             :element-type %element-type)))
      (cffi:with-pointer-to-vector-data (ptr data)
        (%gl:get-buffer-sub-data target %offset (* count %byte-stride) ptr))
      (ecase %type
        (:scalar (%read-buffer-member/scalar member data count))
        (:vec (%read-buffer-member/vector member data count))
        (:mat (%read-buffer-member/matrix member data count))))))

(defun read-buffer-path (buffer-name path &optional count)
  (with-slots (%id %target %layout) (find-buffer buffer-name)
    (let ((member (u:href (members %layout) path)))
      (gl:bind-buffer %target %id)
      (unwind-protect (%read-buffer-member %target member count)
        (gl:bind-buffer %target 0)))))
