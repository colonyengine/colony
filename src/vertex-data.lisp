(in-package :first-light)

(defmacro define-vertex-layout (&body body)
  body)

(defmacro define-vertex-spec (&body body)
  body)

(defmacro define-mesh (&body body)
  body)

(defclass vertex-metadata ()
  ((%layouts :accessor layouts)))

(defclass vertex-layout-block ()
  ((%id :reader id
        :initarg :id)
   (%divisor :reader divisor
             :initarg :divisor)
   (%attrs :reader attrs
           :initarg :attrs)))

(defun %make-layout-block (id divisor attrs)
  (make-instance 'vertex-layout-block :id id :divisor divisor :attrs attrs))

(defun %collect-layouts (path)
  (let ((layouts (make-hash-table)))
    (dolist (form (collect-extension-forms 'vertex path))
      (when (eq (car form) 'define-vertex-layout)
        (destructuring-bind (form-type name blocks) form
          (declare (ignore form-type))
          (dolist (spec blocks)
            (destructuring-bind (&key (id :mesh-data) (divisor 1) attrs) spec
              (let ((layout-block (%make-layout-block id divisor attrs)))
                (appendf (gethash name layouts) (list layout-block))))))))
    layouts))

(defun %create-vertex-spec (id attrs)
  (let ((kit.gl.vao::*vao-decl* (make-instance 'kit.gl.vao::vao-declaration)))
    (map nil
         (lambda (x)
           (kit.gl.vao::vao-add kit.gl.vao::*vao-decl*
                                (kit.gl.vao::vao-parse x)))
         `((:interleave () ,@attrs))) ; insert correct form here
    (setf (gethash id kit.gl.vao::*vao-declarations*) kit.gl.vao::*vao-decl*)))

(defmethod extension-file-type ((extension-type (eql 'vertex)))
  "vert")

(defmethod prepare-extension ((extension-type (eql 'vertex)) owner path)
  (let ((data (make-instance 'vertex-metadata)))
    (with-accessors ((layouts layouts)) data
      (setf layouts (%collect-layouts path)
            (vertex-metadata owner) data))))

;;; TODO

;; we need to collect all the different blocks across all layouts
;; '(((pos :float 3) (uv :float 3))
;;   ((normal :float 3))
;;   ((color :float 4)))
;; and also the divisors for each block, etc
;; then generate the form for glkit, pseudocode:
;; (mapcan (lambda (x) `((:interleave (divisor) ,@attrs))) the-list)
