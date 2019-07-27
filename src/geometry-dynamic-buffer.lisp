(in-package #:%first-light)

(defgeneric get-geometry-group-buffer-names (spec group))

(defmethod get-geometry-group-buffer-names (spec
                                            (group geometry-group/interleaved))
  (list (name group)))

(defmethod get-geometry-group-buffer-names (spec
                                            (group geometry-group/separate))
  (let (names)
    (u:do-hash-keys (k (attributes group))
      (push (a:format-symbol :keyword "~a/~a" (name group) k) names))
    (nreverse names)))

(defun make-geometry-buffers (geometry)
  (with-slots (%layout %buffers %buffer-names) geometry
    (with-slots (%groups %group-order) %layout
      (setf %buffers (make-array 0 :fill-pointer 0 :adjustable t))
      (dolist (group-name %group-order)
        (let ((group (u:href %groups group-name)))
          (dolist (name (get-geometry-group-buffer-names %layout group))
            (let ((buffer (gl:gen-buffer)))
              (setf (u:href %buffer-names name) buffer)
              (vector-push-extend buffer %buffers))))))))

(defun configure-geometry-buffers (geometry)
  (with-slots (%groups %group-order) (layout geometry)
    (let ((buffer-offset 0)
          (attr-offset 0))
      (dolist (group-name %group-order)
        (let* ((group (u:href %groups group-name))
               (buffer-count (get-geometry-group-buffer-count group))
               (group-buffers (make-array
                               buffer-count
                               :displaced-to (buffers geometry)
                               :displaced-index-offset buffer-offset))
               (attr-count (length (attribute-order group))))
          (dotimes (i attr-count)
            (gl:enable-vertex-attrib-array (+ attr-offset i)))
          (configure-geometry-group group attr-offset group-buffers)
          (incf buffer-offset buffer-count)
          (incf attr-offset attr-count))))))

(defun get-geometry-buffer-size (buffer)
  (* (length buffer)
     (etypecase buffer
       ((simple-array (signed-byte 8) *) 1)
       ((simple-array (unsigned-byte 8) *) 1)
       ((simple-array (signed-byte 16) *) 2)
       ((simple-array (unsigned-byte 16) *) 2)
       ((simple-array (signed-byte 32) *) 4)
       ((simple-array (unsigned-byte 32) *) 4)
       ((simple-array single-float *) 4)
       ((simple-array double-float *) 8))))

(defmacro with-geometry-buffer ((ptr size vector) &body body)
  (a:with-gensyms (sv)
    `(static-vectors:with-static-vector
         (,sv (length ,vector)
              :element-type (array-element-type ,vector)
              :initial-contents ,vector)
       (let ((,size (get-geometry-buffer-size ,vector))
             (,ptr (static-vectors:static-vector-pointer ,sv)))
         ,@body))))

(defun fill-geometry-buffer (geometry buffer-name data
                             &key (usage :dynamic-draw))
  (with-geometry-buffer (ptr size (u:flatten-numbers data))
    (let ((buffer (u:href (buffer-names geometry) buffer-name)))
      (gl:bind-buffer :array-buffer buffer)
      (%gl:buffer-data :array-buffer size ptr usage))))

(defun draw-dynamic-geometry (geometry primitive-count &key (first 0) count)
  (with-slots (%primitive %vertex-count) geometry
    (%gl:draw-arrays-instanced
     %primitive
     first
     (or count %vertex-count)
     primitive-count)))
