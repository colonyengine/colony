(in-package :first-light)

(define-component mesh ()
  (mesh-data nil) ; not used yet
  (vao nil))

(defmethod initialize-component ((component mesh) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (unless store
      (setf store (test-make-vao)))
    (setf (vao component) store)))

(defun write-buffer-data (vao vbo vertex-data)
  (let ((data (flatten-numbers vertex-data)))
    (kit.gl.vao:vao-buffer-vector vao vbo data)))

;;; test

(defparameter *test-vertex-data*
  '(((0.5 0.5) (1 1) (1 0 0 1))
    ((-0.5 0.5) (0 1) (0 1 0 1))
    ((-0.5 -0.5) (0 0) (0 0 1 1))
    ((0.5 0.5) (1 1) (1 0 0 1))
    ((-0.5 -0.5) (0 0) (0 0 1 1))
    ((0.5 -0.5) (1 0) (0 1 0 1))))

(defun test-make-vao ()
  (let ((vao (make-instance 'kit.gl.vao:vao
                            :type '2d/color
                            :primitive :triangles
                            :vertex-count (length *test-vertex-data*))))
    (write-buffer-data vao 0 *test-vertex-data*)
    vao))
