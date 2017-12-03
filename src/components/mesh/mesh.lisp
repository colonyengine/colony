(in-package :first-light)

(define-component mesh ()
  (location nil)
  (layout nil)
  (vao nil))

(defun write-buffer-data (layout vao buffer-id data)
  (let ((index (gethash buffer-id (buffer-indices layout)))
        (data (flatten-numbers data)))
    (kit.gl.vao:vao-buffer-vector vao index data)))

(defun update-mesh-buffer (mesh buffer-id data)
  (with-accessors ((layout layout) (vao vao)) mesh
    (write-buffer-data layout vao buffer-id data)))

(defun make-vao (layout buffers)
  (let ((vao (make-instance 'kit.gl.vao:vao
                            :type (id layout)
                            :primitive (primitive layout)
                            :vertex-count (length (cadar buffers)))))
    (loop :for (id data) :in buffers
          :for index = (gethash id (buffer-indices layout))
          :do (write-buffer-data layout vao id data))
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
            (let* ((buffers (load-mesh context component))
                   (new-vao (make-vao layout buffers))
                   (cached (make-cached-mesh location layout new-vao)))
              (setf (cached-mesh store location) cached
                    layout (layout cached)
                    vao (vao cached))
              (slog:emit :component.mesh.cache.created)))))))
