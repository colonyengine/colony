(in-package :first-light)

(defmacro define-vertex-layout (&body body)
  body)

(defmacro define-vertex-spec (&body body)
  body)

(defmacro define-mesh (&body body)
  body)

(defclass vertex-metadata ()
  ((%layouts :reader layouts)
   (%specs :reader specs)))

(defclass vertex-layout-group ()
  ((%id :reader id
        :initarg :id)
   (%divisor :reader divisor
             :initarg :divisor)
   (%attrs :reader attrs
           :initarg :attrs)))

(defclass vertex-spec ()
  ((%id :reader id
        :initarg :id)
   (%layout-names :reader layout-names
                  :initarg :layout-names)
   (%buffer-order :accessor buffer-order
                  :initform nil)
   (%layout-groups :accessor layout-groups
                   :initform (make-hash-table))
   (%primitive :reader primitive
               :initarg :primitive)
   (%vao-declaration :accessor vao-declaration)))

(defun %make-layout-group (id divisor attrs)
  (make-instance 'vertex-layout-group :id id :divisor divisor :attrs attrs))

(defun %make-vertex-spec (id names primitive)
  (make-instance 'vertex-spec :id id :layout-names names :primitive primitive))

(defun %collect-vertex-layouts (path)
  (let ((layouts (make-hash-table)))
    (dolist (form (collect-extension-forms 'vertex path))
      (when (eq (car form) 'define-vertex-layout)
        (destructuring-bind (form-type name groups) form
          (declare (ignore form-type))
          (dolist (spec groups)
            (destructuring-bind (&key (id :mesh-data) (divisor 0) attrs) spec
              (let ((layout-group (%make-layout-group id divisor attrs)))
                (appendf (gethash name layouts) (list layout-group))))))))
    layouts))

(defun %collect-vertex-specs (path layout-table)
  (let ((specs (make-hash-table)))
    (dolist (form (collect-extension-forms 'vertex path))
      (when (eq (car form) 'define-vertex-spec)
        (destructuring-bind (form-type id body) form
          (declare (ignore form-type))
          (destructuring-bind (&key layouts (primitive :triangles)) body
            (let ((vertex-spec (%make-vertex-spec id layouts primitive)))
              ;; populate the spec groups with additive layout groups
              ;; TODO cleanup this mess

              ;; NOTE it is undefined behavior if a divisor is specified
              ;; multiple times across the same group having different values.
              ;; maybe we want to override it if this occurs? (last layout to
              ;; set it in the additive merge wins).
              (dolist (layout layouts)
                (dolist (group-spec (gethash layout layout-table))
                  (unless (member (id group-spec) (buffer-order vertex-spec))
                    (appendf (buffer-order vertex-spec) (list (id group-spec))))
                  (symbol-macrolet ((groups
                                      (gethash (id group-spec)
                                               (layout-groups vertex-spec))))
                    (dolist (attr (attrs group-spec))
                      (if-let ((existsp (find-if
                                         (lambda (x) (eq (first x) (first attr)))
                                         groups)))
                        (setf (cdr existsp) (copy-seq (cdr attr)))
                        (appendf groups (list attr)))))))
              (setf (vao-declaration vertex-spec) (%create-vao-declaration vertex-spec))
              ;; add the spec to the table
              (setf (gethash id specs) vertex-spec))))))
    specs))

(defun %create-vao-declaration (spec)
  (let ((kit.gl.vao::*vao-decl* (make-instance 'kit.gl.vao::vao-declaration)))
    (map nil
         (lambda (x)
           (kit.gl.vao::vao-add kit.gl.vao::*vao-decl*
                                (kit.gl.vao::vao-parse x)))
         (loop :for buffer-id :in (buffer-order spec)
               :for attrs = (gethash buffer-id (layout-groups spec))
               :nconc `((:interleave (:divisor 0) ,@attrs)))) ; TODO: add real group divisor
    (setf (gethash (id spec) kit.gl.vao::*vao-declarations*)
          kit.gl.vao::*vao-decl*)))

(defmethod extension-file-type ((extension-type (eql 'vertex)))
  "vert")

(defmethod prepare-extension ((extension-type (eql 'vertex)) owner path)
  (let ((data (make-instance 'vertex-metadata)))
    (with-slots (%layouts %specs) data
      (setf %layouts (%collect-vertex-layouts path)
            %specs (%collect-vertex-specs path %layouts)
            (vertex-metadata owner) data))))

(defun test (spec-name)
  (loop :with spec = (gethash spec-name (specs first-light-example::*test*))
        :for buffer-id :in (buffer-order spec)
        :for attrs = (gethash buffer-id (layout-groups spec))
        :nconc `((:interleave (:divisor 0) ,@attrs))))
