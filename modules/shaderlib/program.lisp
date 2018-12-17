(in-package :fl.shaderlib)

(defvar *active-shader-program*)

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
            :initform (fl.util:dict #'eq))
   (%primitive :reader primitive
               :initarg :primitive)
   (%stage-specs :reader stage-specs
                 :initarg :stage-specs)
   (%attributes :reader attributes
                :initform (fl.util:dict #'eq))
   (%uniforms :reader uniforms
              :initform (fl.util:dict #'eq))
   (%blocks :reader blocks
            :initform (fl.util:dict #'equal))))

(defun find-program (program-name)
  (let ((programs (fl.data:get 'programs)))
    (fl.util:href programs program-name)))

(defun view-source (program-name stage)
  (fl.util:when-let ((program (find-program program-name)))
    (format t "~a" (fl.util:href (source program) stage))))

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
           (error "Failed to compile ~a shader stage:~%~a~%" type (gl:get-shader-info-log shader)))))
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
          (gl:link-program program)
          (unless (gl:get-program program :link-status)
            (error "Failed to link shader program: ~a" (gl:get-program-info-log program)))
          (dolist (shader shaders)
            (gl:detach-shader program shader)
            (gl:delete-shader shader))))
    program))

(defun build-shader-program (name)
  "Compile the shader stages of NAME, linking them into a program. NAME refers to a previously
defined shader program using MAKE-SHADER-PROGRAM.

See MAKE-SHADER-PROGRAM"
  (let* ((program (find-program name))
         (shaders (compile-stages program))
         (id (link-program shaders)))
    (setf (slot-value program '%id) id)
    (store-attribute-locations program)
    (store-uniform-locations program)
    id))

(defun build-shader-dictionary ()
  "Compile all shader programs defined with MAKE-SHADER-PROGRAM.

See MAKE-SHADER-PROGRAM"
  (let ((programs (fl.data:get 'programs)))
    (fl.util:maphash-keys #'build-shader-program programs)
    programs))

(defun store-stage-program-dependencies (program)
  (let ((stage-fn->programs (fl.data:get 'stage-fn->programs)))
    (dolist (stage-spec (stage-specs program))
      (destructuring-bind (stage-type func-spec) stage-spec
        (declare (ignore stage-type))
        (pushnew (name program) (fl.util:href stage-fn->programs func-spec))))))

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
    (setf (fl.util:href programs name) program)
    (translate-program program)
    (store-attributes program)
    (store-uniforms program)
    (store-stage-program-dependencies program)
    program))

(defmacro define-shader (name (&key (version :330) (primitive :triangles)) &body body)
  "Create a new shader program using the stage-specs defined in BODY.

VERSION: The default version shader stages use, and can be overridden on a per-function basis.

PRIMITIVE: The drawing primitive to use for the vertex stage."
  `(%make-shader-program ',name ,version ,primitive ',body))

(defun translate-shader-programs (program-list)
  "Re-translate a collection of shader programs."
  (dolist (program-name program-list)
    (let ((program (find-program program-name)))
      (translate-program program))))

(defun build-shader-programs (program-list)
  "Recompile a collection of shader programs."
  (dolist (program-name program-list)
    (build-shader-program program-name)))

(defun set-modify-hook (function)
  "Specify a function to be called when shader programs need to be updated."
  (fl.data:set 'modify-hook function))

(defmacro with-shader-program (name &body body)
  "Run a body of code which uses (as in glUseProgram) the program identified by NAME."
  `(let ((*active-shader-program* (find-program ,name)))
     (gl:use-program (id *active-shader-program*))
     ,@body
     (gl:use-program 0)))

