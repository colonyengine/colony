(in-package :fl.assets)

(fu:define-constant +attribute-locations+
    '(("POSITION" . 0)
      ("NORMAL" . 1)
      ("TANGENT" . 2)
      ("COLOR_0" . 3)
      ("TEXCOORD_0" . 4)
      ("TEXCOORD_1" . 5)
      ("JOINTS_0" . 6)
      ("WEIGHTS_0" . 7))
  :test #'equal)

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

(defclass primitive ()
  ((%vao :reader vao
         :initarg :vao)
   (%mode :reader mode
          :initarg :mode)
   (%count :accessor element-count)
   (%type :accessor component-type)
   (%index-buffer :accessor index-buffer)
   (%draw-func :accessor draw-func)))

(fu:define-printer (gltf-chunk stream :type t)
  (format stream "~s" (chunk-type gltf-chunk)))

(defun get-property (gltf key &optional object)
  (let ((object (or object (json gltf))))
    (when (jsown:keyp object key)
      (jsown:val (or object (json gltf)) key))))

(defun chunk-type (chunk)
  (case (%chunk-type chunk)
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

(defgeneric parse-chunk-data (gltf chunk-type chunk)
  (:method :around (gltf chunk-type chunk)
    (parsley:with-buffer-read (:sequence (parsley:read-bytes (chunk-length chunk)))
      (call-next-method))))

(defmethod parse-chunk-data (gltf (chunk-type (eql :json-content)) chunk)
  (let ((data (parsley:read-string :encoding :utf-8)))
    (setf (json gltf) (jsown:parse data))
    data))

(defmethod parse-chunk-data (gltf (chunk-type (eql :binary-buffer)) chunk)
  (loop :with buffers = (get-property gltf "buffers")
        :with data = (make-array (length buffers))
        :for buffer :in buffers
        :for index :below (length buffers)
        :for size = (get-property gltf "byteLength" buffer)
        :do (setf (aref data index) (parsley:read-bytes size))
        :finally (setf (buffers gltf) data))
  nil)

(defmethod parse-chunk-data (gltf (chunk-type (eql :unknown)) chunk)
  (warn "Ignoring an unknown chunk type."))

(defun parse-chunk (gltf)
  (let ((chunk (make-instance 'gltf-chunk)))
    (with-slots (%length %type %data) chunk
      (setf %length (parsley:read-uint-le 4)
            %type (parsley:read-uint-le 4)
            %data (parse-chunk-data gltf (chunk-type chunk) chunk)))
    chunk))

(defun parse-chunks (gltf)
  (loop :with stream = (parsley:buffer-stream)
        :until (= (file-position stream) (file-length stream))
        :for chunk = (parse-chunk gltf)
        :collect chunk))

(defun parse-datastream (gltf)
  (let ((datastream (make-instance 'gltf-datastream)))
    (with-slots (%header %chunks) datastream
      (setf %header (parse-header)
            %chunks (parse-chunks gltf)))
    datastream))

(defun load-gltf (path)
  (fu:with-binary-input (in path)
    (parsley:with-buffer-read (:stream in)
      (let ((gltf (make-instance 'gltf)))
        (setf (parse-tree gltf) (parse-datastream gltf))
        gltf))))

(defun get-component-type (gltf accessor)
  (ecase (get-property gltf "componentType" accessor)
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun get-component-count (data-type)
  (ecase (fu:make-keyword data-type)
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun get-attribute-location (name)
  (fu:alist-get +attribute-locations+ name :test #'string=))

(defun get-attribute-normalization (name component-type)
  (if (and (or (eq component-type :unsigned-byte)
               (eq component-type :unsigned-short))
           (not (string= name "JOINTS_0")))
      :true
      :false))

(defun get-primitive-mode (gltf primitive)
  (case (get-property gltf "mode" primitive)
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)
    (otherwise :triangles)))

(defun find-mesh (gltf name/index)
  (let ((meshes (get-property gltf "meshes")))
    (flet ((%find-by-name (name)
             (or (find-if
                  (lambda (x)
                    (string= (get-property gltf "name" x) name))
                  meshes)
                 (first meshes))))
      (get-property
       gltf
       "primitives"
       (etypecase name/index
         (integer (elt meshes name/index))
         (string (%find-by-name name/index)))))))

(defun make-gpu-buffer (gltf target accessor)
  (let ((buffer-view-index (get-property gltf "bufferView" accessor)))
    (unless (member buffer-view-index (allocated-views gltf))
      (let* ((buffer-view (elt (get-property gltf "bufferViews") buffer-view-index))
             (index (get-property gltf "buffer" buffer-view))
             (offset (+ (or (get-property gltf "byteOffset" accessor) 0)
                        (or (get-property gltf "byteOffset" buffer-view) 0)))
             (size (get-property gltf "byteLength" buffer-view))
             (buffer (aref (buffers gltf) index))
             (data (static-vectors:make-static-vector
                    size
                    :element-type 'fu:octet
                    :initial-contents (subseq buffer offset (+ offset size))))
             (pointer (static-vectors:static-vector-pointer data))
             (buffer-id (gl:gen-buffer)))
        (gl:bind-buffer target buffer-id)
        (%gl:buffer-data target size pointer :static-draw)
        (static-vectors:free-static-vector data)
        (push buffer-view-index (allocated-views gltf))
        buffer-id))))

(defun configure-attribute (gltf attribute accessor)
  (let* ((buffer-view (elt (get-property gltf "bufferViews")
                           (get-property gltf "bufferView" accessor)))
         (type (get-component-type gltf accessor))
         (count (get-component-count (get-property gltf "type" accessor)))
         (stride (or (get-property gltf "byteStride" buffer-view) 0))
         (location (get-attribute-location attribute))
         (normalize (get-attribute-normalization attribute type)))
    (gl:enable-vertex-attrib-array location)
    (%gl:vertex-attrib-pointer location count type normalize stride 0)))

(defun make-vertex-buffers (gltf primitive data)
  (jsown:do-json-keys (attr accessor-id) (get-property gltf "attributes" data)
    (let ((accessor (elt (get-property gltf "accessors") accessor-id)))
      (make-gpu-buffer gltf :array-buffer accessor)
      (configure-attribute gltf attr accessor)
      (with-slots (%vao %mode %count %draw-func) primitive
        (when (string= attr "POSITION")
          (setf %count (get-property gltf "count" accessor)
                %draw-func (lambda (&key (instance-count 1))
                             (gl:bind-vertex-array %vao)
                             (gl:draw-arrays-instanced %mode 0 %count instance-count))))))))

(defun make-index-buffer (gltf primitive data)
  (fu:when-let* ((indices (get-property gltf "indices" data))
                 (accessor (elt (get-property gltf "accessors") indices)))
    (with-slots (%vao %mode %count %type %index-buffer %draw-func) primitive
      (setf %count (get-property gltf "count" accessor)
            %type (get-component-type gltf accessor)
            %index-buffer (make-gpu-buffer gltf :element-array-buffer accessor)
            %draw-func (lambda (&key (instance-count 1))
                         (gl:bind-vertex-array %vao)
                         (gl:bind-buffer :element-array-buffer %index-buffer)
                         (%gl:draw-elements-instanced %mode %count %type 0 instance-count))))))

(defun make-primitive (gltf data)
  (let ((primitive (make-instance 'primitive
                                  :vao (gl:gen-vertex-array)
                                  :mode (get-primitive-mode gltf data))))
    (gl:bind-vertex-array (vao primitive))
    (make-vertex-buffers gltf primitive data)
    (make-index-buffer gltf primitive data)
    primitive))

(defun load-mesh (path mesh-id)
  (let ((gltf (load-gltf path)))
    (dolist (primitive-data (find-mesh gltf mesh-id))
      (push (make-primitive gltf primitive-data) (primitives gltf)))
    (setf (buffers gltf) nil)
    (primitives gltf)))
