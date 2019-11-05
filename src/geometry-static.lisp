(in-package #:virality.geometry)

(a:define-constant +attribute-locations+
    ;; NOTE: The integers are arbitrarily assigned for our engine. However,
    ;; the umbra MESH-ATTRS structure MUST agree with the ordering. As in,
    ;; if "TANGENT" is 2, then the 3rd slot in umbra's MESH-ATTRS struct must
    ;; be mesh/tangent.
    '(("POSITION" . 0)
      ("NORMAL" . 1)
      ("TANGENT" . 2)
      ("COLOR_0" . 3)
      ("TEXCOORD_0" . 4)
      ("TEXCOORD_1" . 5)
      ("JOINTS_0" . 6)
      ("WEIGHTS_0" . 7))
  :test #'equal)

(defclass gltf-primitive ()
  ((%vao :reader vao
         :initarg :vao)
   (%mode :reader mode
          :initarg :mode)
   (%count :accessor element-count)
   (%type :accessor component-type)
   (%index-buffer :accessor index-buffer)
   (%draw-func :accessor draw-func)))

(defun gltf-attribute-name->attribute-location (name)
  (u:alist-get +attribute-locations+ name :test #'string=))

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
         (location (gltf-attribute-name->attribute-location attribute))
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
