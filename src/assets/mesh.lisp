(in-package :fl.assets)

(defvar *attribute-locations*
  '(("POSITION" . 0)
    ("NORMAL" . 1)
    ("TANGENT" . 2)
    ("COLOR_0" . 3)
    ("TEXCOORD_0" . 4)
    ("TEXCOORD_1" . 5)
    ("JOINTS_0" . 6)
    ("WEIGHTS_0" . 7)))

(defclass primitive ()
  ((%vao :reader vao
         :initarg :vao)
   (%mode :reader mode
          :initarg :mode)
   (%count :accessor element-count)
   (%type :accessor component-type)
   (%index-buffer :accessor index-buffer)
   (%draw-func :accessor draw-func)))

(defun get-component-type (accessor)
  (ecase (get-property "componentType" accessor)
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun get-component-count (data-type)
  (ecase (alexandria:make-keyword data-type)
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun get-attribute-location (name)
  (cdr (assoc name *attribute-locations* :test #'string=)))

(defun get-attribute-normalization (name component-type)
  (if (and (or (eq component-type :unsigned-byte)
               (eq component-type :unsigned-short))
           (not (string= name "JOINTS_0")))
      :true
      :false))

(defun get-primitive-mode (primitive)
  (case (get-property "mode" primitive)
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)
    (otherwise :triangles)))

(defun find-mesh (name/index)
  (let ((meshes (get-property "meshes")))
    (flet ((%find-by-name (name)
             (or
              (find-if
               (lambda (x) (string= (get-property "name" x) name))
               meshes)
              (first meshes))))
      (get-property
       "primitives"
       (etypecase name/index
         (integer (elt meshes name/index))
         (string (%find-by-name name/index)))))))

(defun make-gpu-buffer (target accessor)
  (let ((buffer-view-index (get-property "bufferView" accessor)))
    (unless (member buffer-view-index (allocated-views *object*))
      (let* ((buffer-view (elt (get-property "bufferViews") buffer-view-index))
             (index (get-property "buffer" buffer-view))
             (offset (+ (or (get-property "byteOffset" accessor) 0)
                        (or (get-property "byteOffset" buffer-view) 0)))
             (size (get-property "byteLength" buffer-view))
             (buffer (aref (buffers *object*) index))
             (data (static-vectors:make-static-vector
                    size
                    :element-type '(unsigned-byte 8)
                    :initial-contents (subseq buffer offset (+ offset size))))
             (pointer (static-vectors:static-vector-pointer data))
             (buffer-id (gl:gen-buffer)))
        (gl:bind-buffer target buffer-id)
        (%gl:buffer-data target size pointer :static-draw)
        (static-vectors:free-static-vector data)
        (push buffer-view-index (allocated-views *object*))
        buffer-id))))

(defun configure-attribute (attribute accessor)
  (let* ((buffer-view (elt (get-property "bufferViews")
                           (get-property "bufferView" accessor)))
         (type (get-component-type accessor))
         (count (get-component-count (get-property "type" accessor)))
         (stride (or (get-property "byteStride" buffer-view) 0))
         (location (get-attribute-location attribute))
         (normalize (get-attribute-normalization attribute type)))
    (gl:enable-vertex-attrib-array location)
    (%gl:vertex-attrib-pointer location count type normalize stride 0)))

(defun make-vertex-buffers (primitive data)
  (jsown:do-json-keys (attr accessor-id) (get-property "attributes" data)
    (let ((accessor (elt (get-property "accessors") accessor-id)))
      (make-gpu-buffer :array-buffer accessor)
      (configure-attribute attr accessor)
      (with-slots (%vao %mode %count %draw-func) primitive
        (when (string= attr "POSITION")
          (setf %count (get-property "count" accessor)
                %draw-func
                (lambda (&key (instance-count 1))
                  (gl:bind-vertex-array %vao)
                  (gl:draw-arrays-instanced %mode 0 %count instance-count))))))))

(defun make-index-buffer (primitive data)
  (alexandria:when-let* ((indices (get-property "indices" data))
              (accessor (elt (get-property "accessors") indices)))
    (with-slots (%vao %mode %count %type %index-buffer %draw-func) primitive
      (setf %count (get-property "count" accessor)
            %type (get-component-type accessor)
            %index-buffer (make-gpu-buffer :element-array-buffer accessor)
            %draw-func
            (lambda (&key (instance-count 1))
              (gl:bind-vertex-array %vao)
              (gl:bind-buffer :element-array-buffer %index-buffer)
              (%gl:draw-elements-instanced %mode %count %type 0 instance-count))))))

(defun make-primitive (data)
  (let ((primitive (make-instance 'primitive
                                  :vao (gl:gen-vertex-array)
                                  :mode (get-primitive-mode data))))
    (gl:bind-vertex-array (vao primitive))
    (make-vertex-buffers primitive data)
    (make-index-buffer primitive data)
    primitive))

(defun load-mesh (path mesh-id)
  (let ((*object* (load-gltf path)))
    (dolist (primitive-data (find-mesh mesh-id))
      (push (make-primitive primitive-data) (primitives *object*)))
    (setf (buffers *object*) nil)
    (primitives *object*)))
