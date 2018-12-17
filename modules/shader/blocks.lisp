(in-package :first-light.shader)

(defclass shader-block ()
  ((%id :reader id
        :initarg :id)
   (%name :reader name
          :initarg :name)
   (%type :reader block-type
          :initarg :type)
   (%layout :reader layout
            :initarg :layout)
   (%program :reader program
             :initarg :program)
   (%binding-point :reader binding-point
                   :initform 0)))

(fl.util:define-printer (shader-block stream :type nil)
  (format stream "BLOCK ~s" (id shader-block)))

(cl:defun get-block-type (struct)
  (cond
    ((has-qualifier-p struct :ubo)
     (values :uniform :ubo))
    ((has-qualifier-p struct :ssbo)
     (values :buffer :ssbo))))

(cl:defun make-block (program layout)
  (let* ((uniform (uniform layout))
         (id (ensure-keyword (varjo:name uniform)))
         (name (varjo.internals:safe-glsl-name-string id))
         (type (varjo:v-type-of uniform)))
    (fl.util:mvlet ((block-type buffer-type (get-block-type type)))
      (setf (fl.util:href (blocks program) (cons block-type id))
            (make-instance 'shader-block
                           :id id
                           :name (format nil "_~a_~a" buffer-type name)
                           :type block-type
                           :layout layout
                           :program program)))))

(cl:defun create-block-alias (block-type block-id program-name block-alias)
  (let* ((program-name (name (find-program program-name)))
         (aliases (fl.data:get 'block-aliases))
         (block (%find-block program-name block-type block-id)))
    (if (fl.util:href aliases block-alias)
        (error "The block alias ~s is already in use." block-alias)
        (setf (fl.util:href aliases block-alias) block))))

(cl:defun delete-block-alias (block-alias &key unbind-block)
  (when unbind-block
    (unbind-block block-alias))
  (remhash block-alias (fl.data:get 'block-aliases)))

(cl:defun store-blocks (program stage)
  (dolist (layout (collect-layouts stage))
    (make-block program layout)))

(cl:defun %find-block (program-name block-type block-id)
  (if (keywordp block-id)
      (fl.util:when-let ((program (find-program program-name)))
        (fl.util:href (blocks program) (cons block-type block-id)))
      (error "Block ID must be a keyword symbol: ~a" block-id)))

(cl:defun find-block (block-alias)
  (let ((aliases (fl.data:get 'block-aliases)))
    (fl.util:href aliases block-alias)))

(cl:defun block-binding-valid-p (block binding-point)
  (let ((bindings (fl.data:get 'block-bindings)))
    (every
     (lambda (x)
       (varjo:v-type-eq
        (varjo:v-type-of (uniform (layout block)))
        (varjo:v-type-of (uniform (layout x)))))
     (fl.util:href bindings (block-type block) binding-point))))

(defmethod %bind-block ((block-type (eql :uniform)) block binding-point)
  (let* ((program-id (id (program block)))
         (index (%gl:get-uniform-block-index program-id (name block))))
    (%gl:uniform-block-binding program-id index binding-point)))

(defmethod %bind-block ((block-type (eql :buffer)) block binding-point)
  (let* ((program-id (id (program block)))
         (index (gl:get-program-resource-index program-id :shader-storage-block (name block))))
    (%gl:shader-storage-block-binding program-id index binding-point)))

(cl:defun bind-block (block-alias binding-point)
  "Bind a block referenced by BLOCK-ALIAS to a binding point."
  (let* ((bindings (fl.data:get 'block-bindings))
         (block (find-block block-alias)))
    (or (block-binding-valid-p block binding-point)
        (error "Cannot bind a block to a binding point with existing blocks of a different layout."))
    (pushnew block (fl.util:href bindings (block-type block) binding-point))
    (%bind-block (block-type block) block binding-point)
    (setf (slot-value block '%binding-point) binding-point)))

(cl:defun unbind-block (block-alias)
  "Unbind a block with the alias BLOCK-ALIAS."
  (bind-block block-alias 0))

(cl:defun rebind-blocks (programs)
  "Rebind all blocks that are members of PROGRAMS."
  (flet ((rebind (block-type)
           (let* ((bindings (fl.data:get 'block-bindings))
                  (table (fl.util:href bindings block-type)))
             (fl.util:do-hash-values (blocks table)
               (dolist (block blocks)
                 (with-slots (%program %binding-point) block
                   (when (member (name %program) programs)
                     (%bind-block :buffer block %binding-point))))))))
    (rebind :uniform)
    (rebind :buffer)
    (fl.util:noop)))
