(in-package :first-light)

(defmacro define-vertex-layout (&body body)
  body)

(defmacro define-vertex-spec (&body body)
  body)

(defmacro define-mesh (&body body)
  body)

(defclass vertex-metadata ()
  ((%layouts :accessor layouts)))

(defun %collect-layout-forms (path)
  (let ((table (make-hash-table)))
    (loop :for form :in (collect-extension-forms 'vertex path)
          :for (form-type name . blocks) = form
          :when (eq form-type 'define-vertex-layout)
            :do (setf (gethash name table) blocks))
    table))

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
      (setf layouts (%collect-layout-forms path)
            (vertex-metadata owner) data))))

;;; TODO

;; we need to collect all the different blocks across all layouts
;; '(((pos :float 3) (uv :float 3))
;;   ((normal :float 3))
;;   ((color :float 4)))
;; and also the divisors for each block, etc
;; then generate the form for glkit, pseudocode:
;; (mapcan (lambda (x) `((:interleave (divisor) ,@attrs))) the-list)
