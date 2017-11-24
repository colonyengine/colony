(in-package :first-light)

(define-component mesh ()
  (mesh-data nil) ; not used yet
  (vao nil))

(defmethod initialize-component ((component mesh) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (unless store
      (setf store (test-make-vao)))
    (setf (vao component) store)))

(defun flatten-floats (sequence &key (type 'float))
  (flet ((coerce/flatten (sequence)
           (mapcar
            (lambda (x) (coerce x type))
            (remove-if (complement #'realp) (flatten sequence)))))
    (let ((sequence (coerce/flatten sequence)))
      (make-array (length sequence) :element-type type
                                    :initial-contents sequence))))

(defun write-buffer-data (vao vbo vertex-data)
  (let ((data (flatten-floats vertex-data :type 'single-float)))
    (kit.gl.vao:vao-buffer-vector vao vbo data)))

;;; test

(kit.gl.vao:defvao mesh ()
  (:interleave ()
               (pos :float 3)
               (uv :float 3)
               (color :float 4)))

(defvar *test-vertex-data*
  '(((0.5 0.5 0) (1 1 0) (1 0 0 1))
    ((-0.5 0.5 0) (0 1 0) (0 1 0 1))
    ((-0.5 -0.5 0) (0 0 0) (0 0 1 1))
    ((0.5 0.5 0) (1 1 0) (1 0 0 1))
    ((-0.5 -0.5 0) (0 0 0) (0 0 1 1))
    ((0.5 -0.5 0) (1 0 0) (0 1 0 1))))

(defun test-make-vao ()
  (let ((vao (make-instance 'kit.gl.vao:vao
                            :type 'mesh
                            :primitive :triangles
                            :vertex-count (length *test-vertex-data*))))
    (write-buffer-data vao 0 *test-vertex-data*)
    vao))
