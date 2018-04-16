(in-package :fl.assets)

(defvar *object*)

(defvar *chunk*)

(defclass gltf ()
  ((%parse-tree :accessor parse-tree)
   (%json :accessor json)
   (%buffers :accessor buffers)
   (%allocated-views :accessor allocated-views
                     :initform nil)
   (%primitives :accessor primitives
                :initform nil)))

(defclass gltf-datastream ()
  ((%header :reader header)
   (%chunks :reader chunks)))

(defclass gltf-header ()
  ((%magic :reader format-magic)
   (%version :reader format-version)
   (%length :reader format-length)))

(defclass gltf-chunk ()
  ((%length :reader chunk-length)
   (%type :reader %chunk-type)
   (%data :reader chunk-data)))

(defmethod print-object ((object gltf-chunk) stream)
  (print-unreadable-object (object stream :type t)
    (let ((*chunk* object))
      (format stream "~s" (chunk-type)))))

(defun chunk-type ()
  (case (%chunk-type *chunk*)
    (#x4e4f534a :json-content)
    (#x004e4942 :binary-buffer)
    (otherwise :unknown)))

(defun last-chunk-p ()
  (= (file-length (parsley:buffer-stream))
     (parsley:buffer-position)))

(defun parse-header ()
  (let ((header (make-instance 'gltf-header)))
    (with-slots (%magic %version %length) header
      (parsley:with-buffer-read (:sequence (parsley:read-bytes 12))
        (let ((magic (parsley:read-string :bytes 4)))
          (if (not (string= magic "glTF"))
              (error "Invalid glTF2 file.")
              (setf %magic magic
                    %version (parsley:read-uint-le 4)
                    %length (parsley:read-uint-le 4))))))
    header))

(defgeneric parse-chunk-data (chunk-type)
  (:method :around (chunk-type)
    (parsley:with-buffer-read (:sequence (parsley:read-bytes (chunk-length *chunk*)))
      (call-next-method))))

(defmethod parse-chunk-data ((chunk-type (eql :json-content)))
  (let ((data (parsley:read-string :encoding :utf-8)))
    (setf (json *object*) (jsown:parse data))
    data))

(defmethod parse-chunk-data ((chunk-type (eql :binary-buffer)))
  (loop :with buffers = (get-property "buffers")
        :with data = (make-array (length buffers))
        :for buffer :in buffers
        :for index :below (length buffers)
        :for size = (get-property "byteLength" buffer)
        :do (setf (aref data index) (parsley:read-bytes size))
        :finally (setf (buffers *object*) data))
  nil)

(defmethod parse-chunk-data ((chunk-type (eql :unknown)))
  (warn "Ignoring an unknown chunk type."))

(defun parse-chunk ()
  (let ((*chunk* (make-instance 'gltf-chunk)))
    (with-slots (%length %type %data) *chunk*
      (setf %length (parsley:read-uint-le 4)
            %type (parsley:read-uint-le 4)
            %data (parse-chunk-data (chunk-type))))
    *chunk*))

(defun parse-chunks ()
  (loop :with stream = (parsley:buffer-stream)
        :until (= (file-position stream) (file-length stream))
        :for chunk = (parse-chunk)
        :collect chunk))

(defun parse-datastream ()
  (let ((datastream (make-instance 'gltf-datastream)))
    (with-slots (%header %chunks) datastream
      (setf %header (parse-header)
            %chunks (parse-chunks)))
    datastream))

(defun get-property (key &optional object)
  (let ((object (or object (json *object*))))
    (when (jsown:keyp object key)
      (jsown:val (or object (json *object*)) key))))

(defun load-gltf (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (parsley:with-buffer-read (:stream in)
      (let ((*object* (make-instance 'gltf)))
        (setf (parse-tree *object*) (parse-datastream))
        *object*))))
