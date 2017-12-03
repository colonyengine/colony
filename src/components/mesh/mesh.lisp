(in-package :first-light)

(define-component mesh ()
  (location nil)
  (layout nil)
  (vao nil))

(defun fill-mesh-buffer (mesh buffer-id data)
  (let ((index (gethash buffer-id (buffer-indices (layout mesh))))
        (data (flatten-numbers data)))
    (kit.gl.vao:vao-buffer-vector (vao mesh) index data)))

(defun make-vao (context mesh)
  (let* ((buffers (load-mesh context mesh))
         (vao (make-instance 'kit.gl.vao:vao
                             :type (id (layout mesh))
                             :primitive (primitive (layout mesh))
                             :vertex-count (length (cadar buffers)))))
    (setf (vao mesh) vao)
    (loop :for (id data) :in buffers
          :for index = (gethash id (buffer-indices (layout mesh)))
          :do (fill-mesh-buffer mesh id data))
    vao))

(defmethod initialize-component ((component mesh) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (with-accessors ((location location) (vao vao) (layout layout)) component
      (unless location
        (error "A mesh component must have a location set."))
      (unless store
        (setf store (make-instance 'mesh-shared-storage)))
      (multiple-value-bind (cached presentp) (cached-mesh store location)
        (if presentp
            (progn
              (setf layout (layout cached)
                    vao (vao cached))
              (slog:emit :component.mesh.cache.used))
            (progn
              (setf (cached-mesh store location)
                    (make-cached-mesh context component))
              (slog:emit :component.mesh.cache.created)))))))
