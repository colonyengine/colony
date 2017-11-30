(in-package :first-light)

(define-component mesh ()
  (location nil)
  (layout nil)
  (vao nil))

(defmethod initialize-component ((component mesh) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (unless store
      (setf store (make-vao context component)))
    (setf (vao component) store)))

(defun write-buffer-data (vao vbo vertex-data)
  (let ((data (flatten-numbers vertex-data)))
    (kit.gl.vao:vao-buffer-vector vao vbo data)))

(defun make-vao (context mesh)
  (let* ((buffers (load-mesh context mesh))
         (vao (make-instance 'kit.gl.vao:vao
                             :type (id (layout mesh))
                             :primitive (primitive (layout mesh))
                             :vertex-count (length (cadar buffers)))))
    (loop :for (id data) :in buffers
          :for index = (gethash id (buffer-indices (layout mesh)))
          :do (write-buffer-data vao index data))
    vao))
