(in-package :first-light.gpu)

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
  (let ((buffer-table (fl.data:get 'buffers)))
    (fl.util:href buffer-table buffer-name)))

(defun create-buffer (buffer-name block-alias)
  "Create a buffer of the given TYPE and NAME, using the block BLOCK-ID of PROGRAM-NAME."
  (fl.util:if-let ((block (find-block block-alias)))
    (let* ((buffer-table (fl.data:get 'buffers))
           (type (block-type->buffer-type (block-type block)))
           (target (buffer-type->target type))
           (buffer (%make-buffer buffer-name target (layout block))))
      (with-slots (%id %layout) buffer
        (%gl:bind-buffer target %id)
        (%gl:buffer-data target (size %layout) (cffi:null-pointer) :static-draw)
        (setf (fl.util:href buffer-table buffer-name) buffer)))
    (error "Cannot find the block with alias ~s when attempting to create a buffer." block-alias)))

(defun bind-buffer (buffer-name binding-point)
  "Bind a buffer with name BUFFER-NAME to BINDING-POINT."
  (fl.util:if-let ((buffer (find-buffer buffer-name)))
    (with-slots (%target %id) buffer
      (%gl:bind-buffer-base %target binding-point %id)
      (%gl:bind-buffer %target 0))
    (error "Cannot find buffer ~s." buffer-name)))

(defun unbind-buffer (buffer-name)
  "Unbind a buffer with name BUFFER-NAME."
  (bind-buffer buffer-name 0))

(defun delete-buffer (buffer-name)
  "Delete the buffer having a name of BUFFER-NAME."
  (let ((buffer-table (fl.data:get 'buffers))
        (buffer (find-buffer buffer-name)))
    (unbind-buffer buffer-name)
    (gl:delete-buffers (list (id buffer)))
    (remhash buffer-name buffer-table)))

(defun %write-buffer-member (target member value)
  (with-slots (%element-type %offset %element-stride %byte-stride) member
    (let ((count (length value)))
      (static-vectors:with-static-vector (sv (* count %element-stride)
                                             :element-type %element-type)
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
  (with-slots (%element-type %offset %element-stride %byte-stride %dimensions) member
    (let ((count (length value)))
      (destructuring-bind (columns . rows) %dimensions
        (static-vectors:with-static-vector (sv (* count columns %element-stride)
                                               :element-type %element-type)
          (let ((ptr (static-vectors:static-vector-pointer sv))
                (i 0))
            (map nil
                 (lambda (x)
                   (loop :repeat columns
                         :for j :from i :by %element-stride
                         :for k :by rows
                         :do (replace sv x :start1 j :start2 k :end2 (+ k rows)))
                   (incf i (* columns %element-stride)))
                 value)
            (%gl:buffer-sub-data target %offset (* count %byte-stride) ptr)))))))

(defun write-buffer-path (buffer-name path value)
  "Write VALUE to the buffer with the name BUFFER-NAME, starting at the given PATH.

PATH: A \"dot-separated\" keyword symbol, where each part denotes a member in the buffer's block
layout.

VALUE: A value to write, such as a scalar or matrix depending on the type of the member PATH refers
to. To write to an array, use a sequence of values.

Note: Writing to arrays which contain other aggregate types (other arrays or structures) is not
possible. This is a design decision to allow this library to have a simple \"path-based\" buffer
writing interface."
  (with-slots (%id %target %layout) (find-buffer buffer-name)
    (let ((member (fl.util:href (members %layout) path)))
      (check-type value sequence)
      (gl:bind-buffer %target %id)
      (if (cdr (dimensions member))
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
           (func (fl.util:format-symbol :flm "VEC~a" size)))
      (flet ((make-vec (data index size)
               (let ((args (loop :for i :below size
                                 :collect (aref data (+ index i)))))
                 (apply func args))))
        (if (= count 1)
            (make-vec data 0 size)
            (loop :repeat count
                  :for i :by %element-stride
                  :collect (make-vec data i size)))))))

(defun %read-buffer-member/matrix (member data count)
  (declare (ignore member data count))
  (error "Reading matrix paths is not yet supported."))

(defun %read-buffer-member (target member &optional count)
  (with-slots (%type %count %element-type %offset %byte-stride) member
    (let* ((count (or count %count))
           (data (make-array (* count %byte-stride) :element-type %element-type)))
      (cffi:with-pointer-to-vector-data (ptr data)
        (%gl:get-buffer-sub-data target %offset (* count %byte-stride) ptr))
      (ecase %type
        (:scalar (%read-buffer-member/scalar member data count))
        (:vec (%read-buffer-member/vector member data count))
        (:mat (%read-buffer-member/matrix member data count))))))

(defun read-buffer-path (buffer-name path &optional count)
  (with-slots (%id %target %layout) (find-buffer buffer-name)
    (let ((member (fl.util:href (members %layout) path)))
      (gl:bind-buffer %target %id)
      (unwind-protect (%read-buffer-member %target member count)
        (gl:bind-buffer %target 0)))))
