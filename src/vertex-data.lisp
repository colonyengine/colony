(in-package :first-light)

(defmacro define-vertex-groups (&body body)
  body)

(defmacro define-vertex-layout (&body body)
  body)

(defmacro define-mesh (&body body)
  body)

(defclass vertex-metadata ()
  ((%groups :reader groups
            :initarg :groups)
   (%layouts :reader layouts
             :initarg :layouts)))

(defclass vertex-group ()
  ((%id :reader id
        :initarg :id)
   (%divisor :reader divisor
             :initarg :divisor)
   (%attrs :reader attrs
           :initarg :attrs)))

(defclass vertex-layout ()
  ((%id :reader id
        :initarg :id)
   (%group-names :reader group-names
                 :initarg :group-names)
   (%group-attrs :reader group-attrs
                 :initform (make-hash-table))
   (%buffer-order :reader buffer-order
                  :initform nil)
   (%buffer-divisors :reader buffer-divisors
                     :initform (make-hash-table))
   (%primitive :reader primitive
               :initarg :primitive)
   (%vao-spec :accessor vao-spec)))

(defun %make-vertex-group (id divisor attrs)
  (make-instance 'vertex-group :id id :divisor divisor :attrs attrs))

(defun %make-vertex-layout (id names primitive)
  (make-instance 'vertex-layout :id id :group-names names :primitive primitive))

(defun %collect-vertex-groups (path)
  (let ((group-table (make-hash-table)))
    (dolist (form (collect-extension-forms 'vertex path))
      (when (eq (car form) 'define-vertex-groups)
        (destructuring-bind (form-type name group-forms) form
          (declare (ignore form-type))
          (dolist (group-form group-forms)
            (destructuring-bind (&key (id :mesh-data) (divisor 0) attrs)
                group-form
              (let ((group (%make-vertex-group id divisor attrs)))
                (push group (gethash name group-table)))))
          (reversef (gethash name group-table)))))
    group-table))

(defun %vertex-layout-add-buffer-id (vertex-layout buffer-id)
  (with-slots (%buffer-order) vertex-layout
    (unless (member buffer-id %buffer-order)
      (push buffer-id %buffer-order)
      (reversef %buffer-order))))

(defun %vertex-layout-splice-group (vertex-layout group)
  (symbol-macrolet ((attrs (gethash (id group) (group-attrs vertex-layout))))
    (dolist (attr (attrs group))
      (if-let ((existsp (find-if
                         (lambda (x) (eq (first x) (first attr)))
                         attrs)))
        (setf (cdr existsp) (copy-seq (cdr attr)))
        (appendf attrs (list attr))))))

(defun %vertex-layout-add-buffer-divisor (vertex-layout group)
  (setf (gethash (id group) (buffer-divisors vertex-layout)) (divisor group)))

(defun %vertex-layout-generate (vertex-layout group-table)
  (with-slots (%vao-spec) vertex-layout
    (dolist (group-name (group-names vertex-layout))
      (dolist (group (gethash group-name group-table))
        (%vertex-layout-add-buffer-id vertex-layout (id group))
        (%vertex-layout-add-buffer-divisor vertex-layout group)
        (%vertex-layout-splice-group vertex-layout group)))
    (setf %vao-spec (%create-vao-spec vertex-layout))))

(defun %collect-vertex-layouts (path group-table)
  (let ((layouts (make-hash-table)))
    (dolist (form (collect-extension-forms 'vertex path))
      (when (eq (car form) 'define-vertex-layout)
        (destructuring-bind (form-type id body) form
          (declare (ignore form-type))
          (destructuring-bind (&key groups (primitive :triangles)) body
            (let ((vertex-layout (%make-vertex-layout id groups primitive)))
              (%vertex-layout-generate vertex-layout group-table)
              (setf (gethash id layouts) vertex-layout))))))
    layouts))

(defun %create-vao-spec (layout)
  (let ((kit.gl.vao::*vao-decl* (make-instance 'kit.gl.vao::vao-declaration)))
    (map nil
         (lambda (x)
           (kit.gl.vao::vao-add kit.gl.vao::*vao-decl*
                                (kit.gl.vao::vao-parse x)))
         (loop :for buffer-id :in (buffer-order layout)
               :for attrs = (gethash buffer-id (group-attrs layout))
               :for divisor = (gethash buffer-id (buffer-divisors layout))
               :nconc `((:interleave (:divisor ,divisor) ,@attrs))))
    (setf (gethash (id layout) kit.gl.vao::*vao-declarations*)
          kit.gl.vao::*vao-decl*)))

(defmethod extension-file-type ((extension-type (eql 'vertex)))
  "vert")

(defmethod prepare-extension ((extension-type (eql 'vertex)) owner path)
  (let* ((groups (%collect-vertex-groups path))
         (layouts (%collect-vertex-layouts path groups)))
    (setf (vertex-metadata owner)
          (make-instance 'vertex-metadata :groups groups :layouts layouts))))
