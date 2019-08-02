(in-package #:first-light.gpu)

(defun reset-program-state ()
  (setf (%fl:meta 'programs) (u:dict)
        (%fl:meta 'block-bindings) (u:dict :uniform (u:dict) :buffer (u:dict))
        (%fl:meta 'block-aliases) (u:dict #'equalp)
        (%fl:meta 'buffers) (u:dict)))

(defun enable-dependency-tracking ()
  (setf (%fl:meta 'track-dependencies-p) t))

(defun disable-dependency-tracking ()
  (setf (%fl:meta 'track-dependencies-p) nil))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (u:href (source program) (stage-type stage))
          (subseq source
                  (1+ (position #\newline source))
                  (- (length source) 2)))))

(defun load-shaders (modify-hook)
  (reset-program-state)
  (u:do-hash-values (shader-factory (%fl:meta 'shader-definitions))
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

(setf (%fl:meta 'track-dependencies-p) nil
      (%fl:meta 'fn->deps) (u:dict #'equal)
      (%fl:meta 'dep->fns) (u:dict #'equal)
      (%fl:meta 'stage-fn->programs) (u:dict #'equal)
      (%fl:meta 'modify-hook) (constantly nil)
      (%fl:meta 'shader-definitions) (u:dict))
(reset-program-state)
