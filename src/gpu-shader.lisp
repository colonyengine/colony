(in-package #:virality.gpu)

(defun reset-program-state ()
  (setf (v::meta 'programs) (u:dict)
        (v::meta 'block-bindings) (u:dict :uniform (u:dict) :buffer (u:dict))
        (v::meta 'block-aliases) (u:dict #'equalp)
        (v::meta 'buffers) (u:dict)))

(defun enable-dependency-tracking ()
  (setf (v::meta 'track-dependencies-p) t))

(defun disable-dependency-tracking ()
  (setf (v::meta 'track-dependencies-p) nil))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (u:href (source program) (stage-type stage))
          (subseq source
                  (1+ (position #\newline source))
                  (- (length source) 2)))))

(defun load-shaders (modify-hook)
  (reset-program-state)
  (u:do-hash-values (shader-factory (v::meta 'shader-definitions))
    (funcall shader-factory))
  (enable-dependency-tracking)
  (set-modify-hook modify-hook)
  (build-shader-dictionary))

(defun unload-shaders ()
  (set-modify-hook (constantly nil))
  (disable-dependency-tracking))

(defun recompile-shaders (programs-list)
  (when programs-list
    (translate-shader-programs programs-list)
    (build-shader-programs programs-list)
    (rebind-blocks programs-list)
    (log:debug :changeme "Shader programs compiled: ~{~s~^, ~}" programs-list)))

(defmacro define-struct (name &body slots)
  `(varjo:define-vari-struct ,name () ,@slots))

(defmacro define-macro (name lambda-list &body body)
  `(varjo:define-vari-macro ,name ,lambda-list ,@body))

(setf (v::meta 'track-dependencies-p) nil
      (v::meta 'fn->deps) (u:dict #'equal)
      (v::meta 'dep->fns) (u:dict #'equal)
      (v::meta 'stage-fn->programs) (u:dict #'equal)
      (v::meta 'modify-hook) (constantly nil)
      (v::meta 'shader-definitions) (u:dict))
(reset-program-state)
