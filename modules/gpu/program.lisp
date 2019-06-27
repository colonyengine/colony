(in-package #:first-light.gpu)

(defclass program ()
  ((%id :reader id
        :initform 0)
   (%name :reader name
          :initarg :name)
   (%version :reader version
             :initarg :version)
   (%translated-stages :reader translated-stages
                       :initform nil)
   (%source :reader source
            :initform (u:dict))
   (%primitive :reader primitive
               :initarg :primitive)
   (%stage-specs :reader stage-specs
                 :initarg :stage-specs)
   (%attributes :reader attributes
                :initform (u:dict))
   (%uniforms :reader uniforms
              :initform (u:dict))
   (%blocks :reader blocks
            :initform (u:dict #'equal))))

(defun find-program (program-name)
  (let ((programs (fl.data:get 'programs)))
    (u:href programs program-name)))

(defun view-source (program-name stage)
  (a:when-let ((program (find-program program-name)))
    (format t "~a" (u:href (source program) stage))))

(defun compile-stages (program)
  (let ((shaders))
    (maphash
     (lambda (k v)
       (let* ((type (stage-type->shader-type k))
              (shader (gl:create-shader type)))
         (gl:shader-source shader v)
         (gl:compile-shader shader)
         (push shader shaders)
         (unless (gl:get-shader shader :compile-status)
           (error "Failed to compile ~a shader stage:~%~a~%"
                  type (gl:get-shader-info-log shader)))))
     (source program))
    shaders))

(defun link-program (shaders)
  (let ((program (gl:create-program)))
    (if (zerop program)
        (progn
          (dolist (shader shaders)
            (gl:delete-shader shader))
          (error "Failed to create program: ~a" (gl:get-error)))
        (progn
          (dolist (shader shaders)
            (gl:attach-shader program shader))
          (without-float-traps
            (gl:link-program program))
          (unless (gl:get-program program :link-status)
            (error "Failed to link shader program: ~a"
                   (gl:get-program-info-log program)))
          (dolist (shader shaders)
            (gl:detach-shader program shader)
            (gl:delete-shader shader))))
    program))

(defun build-shader-program (name)
  (let* ((program (find-program name))
         (shaders (compile-stages program))
         (id (link-program shaders)))
    (setf (slot-value program '%id) id)
    (store-attribute-locations program)
    (store-uniform-locations program)
    id))

(defun build-shader-dictionary ()
  (let ((programs (fl.data:get 'programs)))
    (a:maphash-keys #'build-shader-program programs)
    programs))

(defun store-stage-program-dependencies (program)
  (let ((stage-fn->programs (fl.data:get 'stage-fn->programs)))
    (dolist (stage-spec (stage-specs program))
      (destructuring-bind (stage-type func-spec) stage-spec
        (declare (ignore stage-type))
        (pushnew (name program) (u:href stage-fn->programs func-spec))))))

(defun translate-program (program)
  (with-slots (%name %version %primitive %stage-specs) program
    (let ((stages (translate-stages %version %primitive %stage-specs)))
      (dolist (stage stages)
        (store-source program stage)
        (store-blocks program stage))
      (setf (slot-value program '%translated-stages) stages))))

(defun %make-shader-program (name version primitive stage-specs)
  (let ((programs (fl.data:get 'programs))
        (program (make-instance 'program
                                :name name
                                :version version
                                :primitive primitive
                                :stage-specs stage-specs)))
    (setf (u:href programs name) program)
    (translate-program program)
    (store-attributes program)
    (store-uniforms program)
    (store-stage-program-dependencies program)
    program))

(defmacro define-shader (name (&key (version :430) (primitive :triangles))
                         &body body)
  (let ((definitions '(fl.data:get 'shader-definitions)))
    `(progn
       (setf (u:href ,definitions ',name)
             (lambda ()
               (%make-shader-program ',name ,version ,primitive ',body)))
       (export ',name))))

(defun translate-shader-programs (program-list)
  (dolist (program-name program-list)
    (let ((program (find-program program-name)))
      (translate-program program))))

(defun build-shader-programs (program-list)
  (dolist (program-name program-list)
    (build-shader-program program-name)))

(defun set-modify-hook (function)
  (fl.data:set 'modify-hook function))

(defmacro with-shader (name &body body)
  `(unwind-protect
        (progn
          (gl:use-program (id (find-program ,name)))
          ,@body)
     (gl:use-program 0)))
