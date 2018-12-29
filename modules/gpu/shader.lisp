(in-package :first-light.gpu)

(defun reset-program-state ()
  (fl.data:set 'programs (fl.util:dict #'eq))
  (fl.data:set 'block-bindings (fl.util:dict #'eq
                                             :uniform (fl.util:dict #'eq)
                                             :buffer (fl.util:dict #'eq)))
  (fl.data:set 'block-aliases (fl.util:dict #'equalp))
  (fl.data:set 'buffers (fl.util:dict #'eq)))

(defun enable-dependency-tracking ()
  (fl.data:set 'track-dependencies-p t))

(defun disable-dependency-tracking ()
  (fl.data:set 'track-dependencies-p nil))

(defun store-source (program stage)
  (let ((source (varjo:glsl-code stage)))
    (setf (fl.util:href (source program) (stage-type stage))
          (subseq source (1+ (position #\newline source)) (- (length source) 2)))))

(defun load-shaders (modify-hook)
  (reset-program-state)
  (fl.util:do-hash-values (shader-factory (fl.data:get 'shader-definitions))
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
    (v:debug :fl.gpu "Shader programs compiled: ~{~s~^, ~}" programs-list)))

(defmacro define-struct (name &body slots)
  `(varjo:define-vari-struct ,name () ,@slots))

(defmacro define-macro (name lambda-list &body body)
  `(varjo:define-vari-macro ,name ,lambda-list ,@body))

(fl.data:set 'track-dependencies-p nil)
(fl.data:set 'fn->deps (fl.util:dict #'equal))
(fl.data:set 'dep->fns (fl.util:dict #'equal))
(fl.data:set 'stage-fn->programs (fl.util:dict #'equal))
(fl.data:set 'modify-hook (constantly nil))
(fl.data:set 'shader-definitions (fl.util:dict #'eq))
(reset-program-state)
