(in-package #:virality.engine)

(a:define-constant +attribute-locations+
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
  ((%buffer :reader buffer
            :initarg :buffer)
   (%parse-tree :accessor parse-tree)
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
   (%type :reader chunk-type)
   (%data :reader chunk-data)))

(defclass gltf-primitive ()
  ((%vao :reader vao
         :initarg :vao)
   (%mode :reader mode
          :initarg :mode)
   (%count :accessor element-count)
   (%type :accessor component-type)
   (%index-buffer :accessor index-buffer)
   (%draw-func :accessor draw-func)))

(u:define-printer (gltf-chunk stream :type t)
  (format stream "~s" (get-gltf-chunk-type gltf-chunk)))

(defun get-gltf-property (gltf key &optional object)
  (let ((object (or object (json gltf))))
    (when (jsown:keyp object key)
      (jsown:val (or object (json gltf)) key))))

(defun get-gltf-chunk-type (chunk)
  (case (chunk-type chunk)
    (#x4e4f534a :json-content)
    (#x004e4942 :binary-buffer)
    (otherwise :unknown)))

(defun parse-gltf-header (gltf)
  (let ((header (make-instance 'gltf-header)))
    (with-slots (%magic %version %length) header
      (let* ((buffer (fast-io:make-input-buffer
                      :vector (read-bytes (buffer gltf) 12)))
             (magic (read-string buffer :bytes 4)))
        (if (not (string= magic "glTF"))
            (error "Invalid glTF2 file.")
            (setf %magic magic
                  %version (read-uint-le buffer 4)
                  %length (read-uint-le buffer 4)))))
    header))

(defgeneric parse-gltf-chunk-data (gltf chunk-type chunk &key)
  (:method :around (gltf chunk-type chunk &key)
    (let ((buffer (fast-io:make-input-buffer
                   :vector (read-bytes (buffer gltf) (chunk-length chunk)))))
      (call-next-method gltf chunk-type chunk :buffer buffer))))

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :json-content)) chunk
                                  &key buffer)
  (let ((data (read-string buffer :encoding :utf-8)))
    (setf (json gltf) (jsown:parse data))
    data))

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :binary-buffer)) chunk
                                  &key buffer)
  (loop :with buffers = (get-gltf-property gltf "buffers")
        :with data = (make-array (length buffers))
        :for data-buffer :in buffers
        :for index :below (length buffers)
        :for size = (get-gltf-property gltf "byteLength" data-buffer)
        :do (setf (aref data index) (read-bytes buffer size))
        :finally (setf (buffers gltf) data))
  nil)

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :unknown)) chunk
                                  &key buffer)
  (declare (ignore buffer))
  (warn "Ignoring an unknown chunk type."))

(defun parse-gltf-chunk (gltf)
  (let ((chunk (make-instance 'gltf-chunk))
        (buffer (buffer gltf)))
    (with-slots (%length %type %data) chunk
      (setf %length (read-uint-le buffer 4)
            %type (read-uint-le buffer 4)
            %data (parse-gltf-chunk-data
                   gltf (get-gltf-chunk-type chunk) chunk)))
    chunk))

(defun parse-gltf-chunks (gltf)
  (loop :with stream = (fast-io:input-buffer-stream (buffer gltf))
        :until (= (file-position stream) (file-length stream))
        :for chunk = (parse-gltf-chunk gltf)
        :collect chunk))

(defun parse-gltf-datastream (gltf)
  (let ((datastream (make-instance 'gltf-datastream)))
    (with-slots (%header %chunks) datastream
      (setf %header (parse-gltf-header gltf)
            %chunks (parse-gltf-chunks gltf)))
    datastream))

(defun load-gltf-file (path)
  (u:with-binary-input (in path)
    (let* ((buffer (fast-io:make-input-buffer :stream in))
           (gltf (make-instance 'gltf :buffer buffer)))
      (setf (parse-tree gltf) (parse-gltf-datastream gltf))
      gltf)))

(defun get-gltf-component-type (gltf accessor)
  (ecase (get-gltf-property gltf "componentType" accessor)
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun get-gltf-component-count (data-type)
  (ecase (u:make-keyword data-type)
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun get-gltf-attribute-location (name)
  (u:alist-get +attribute-locations+ name :test #'string=))

(defun get-gltf-attribute-normalization (name component-type)
  (if (and (or (eq component-type :unsigned-byte)
               (eq component-type :unsigned-short))
           (not (string= name "JOINTS_0")))
      :true
      :false))

(defun get-gltf-primitive-mode (gltf primitive)
  (case (get-gltf-property gltf "mode" primitive)
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)
    (otherwise :triangles)))

(defun find-gltf-mesh (gltf index)
  (let ((meshes (get-gltf-property gltf "meshes")))
    (when (>= index (length meshes))
      (error "Mesh index ~d not found." index))
    (get-gltf-property gltf "primitives" (elt meshes index))))

(defun make-gltf-buffer (gltf target accessor)
  (let ((buffer-view-index (get-gltf-property gltf "bufferView" accessor)))
    (unless (member buffer-view-index (allocated-views gltf))
      (let* ((buffer-view (elt (get-gltf-property gltf "bufferViews")
                               buffer-view-index))
             (index (get-gltf-property gltf "buffer" buffer-view))
             (offset (+ (or (get-gltf-property gltf "byteOffset" accessor) 0)
                        (or (get-gltf-property gltf "byteOffset" buffer-view)
                            0)))
             (size (get-gltf-property gltf "byteLength" buffer-view))
             (buffer (aref (buffers gltf) index))
             (data (static-vectors:make-static-vector
                    size
                    :element-type 'u:octet
                    :initial-contents (subseq buffer offset (+ offset size))))
             (pointer (static-vectors:static-vector-pointer data))
             (buffer-id (gl:gen-buffer)))
        (gl:bind-buffer target buffer-id)
        (%gl:buffer-data target size pointer :static-draw)
        (static-vectors:free-static-vector data)
        (push buffer-view-index (allocated-views gltf))
        buffer-id))))

(defun configure-gltf-attribute (gltf attribute accessor)
  (let* ((buffer-view (elt (get-gltf-property gltf "bufferViews")
                           (get-gltf-property gltf "bufferView" accessor)))
         (type (get-gltf-component-type gltf accessor))
         (count (get-gltf-component-count
                 (get-gltf-property gltf "type" accessor)))
         (stride (or (get-gltf-property gltf "byteStride" buffer-view) 0))
         (location (get-gltf-attribute-location attribute))
         (normalize (get-gltf-attribute-normalization attribute type)))
    (gl:enable-vertex-attrib-array location)
    (%gl:vertex-attrib-pointer location count type normalize stride 0)))

(defun make-gltf-vertex-buffers (gltf primitive data)
  (jsown:do-json-keys (attr accessor-id)
                      (get-gltf-property gltf "attributes" data)
    (let ((accessor (elt (get-gltf-property gltf "accessors") accessor-id)))
      (make-gltf-buffer gltf :array-buffer accessor)
      (configure-gltf-attribute gltf attr accessor)
      (with-slots (%count) primitive
        (when (string= attr "POSITION")
          (setf %count (get-gltf-property gltf "count" accessor)))))))

(defun make-gltf-index-buffer (gltf primitive data)
  (a:when-let* ((indices (get-gltf-property gltf "indices" data))
                (accessor (elt (get-gltf-property gltf "accessors") indices)))
    (with-slots (%count %type %index-buffer) primitive
      (setf %count (get-gltf-property gltf "count" accessor)
            %type (get-gltf-component-type gltf accessor)
            %index-buffer (make-gltf-buffer
                           gltf :element-array-buffer accessor)))))

(defun draw-primitive/vertices (primitive instance-count)
  (declare (optimize speed))
  (with-slots (%vao %mode %count) primitive
    (gl:bind-vertex-array %vao)
    (gl:draw-arrays-instanced %mode 0 %count instance-count)))

(defun draw-primitive/indexed (primitive instance-count)
  (declare (optimize speed))
  (with-slots (%vao %index-buffer %mode %count %type) primitive
    (gl:bind-vertex-array %vao)
    (gl:bind-buffer :element-array-buffer %index-buffer)
    (%gl:draw-elements-instanced %mode %count %type 0 instance-count)))

(defun make-draw-func (primitive)
  (with-slots (%index-buffer %draw-func) primitive
    (setf %draw-func (if %index-buffer
                         (lambda (x)
                           (draw-primitive/indexed primitive x))
                         (lambda (x)
                           (draw-primitive/vertices primitive x))))))

(defun make-gltf-primitive (gltf data)
  (let ((primitive (make-instance 'gltf-primitive
                                  :vao (gl:gen-vertex-array)
                                  :mode (get-gltf-primitive-mode gltf data))))
    (with-slots (%index-buffer %draw-func) primitive
      (gl:bind-vertex-array (vao primitive))
      (make-gltf-vertex-buffers gltf primitive data)
      (make-gltf-index-buffer gltf primitive data)
      (make-draw-func primitive)
      primitive)))

(defun load-static-geometry (path mesh-id)
  (let ((gltf (load-gltf-file path)))
    (dolist (primitive-data (find-gltf-mesh gltf mesh-id))
      (push (make-gltf-primitive gltf primitive-data) (primitives gltf)))
    (setf (buffers gltf) nil)
    gltf))

(defun draw-static-primitive (primitive count)
  (funcall (draw-func primitive) count))

(defun draw-static-geometry (mesh count)
  (dolist (primitive (primitives mesh))
    (draw-static-primitive primitive count)))
