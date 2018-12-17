(in-package :fl.shaderlib)

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

(defmacro define-gpu-struct (name () &body slots)
  "Define a GPU structure."
  `(varjo:define-vari-struct ,name () ,@slots))

(defmacro define-gpu-macro (name lambda-list &body body)
  "Define a GPU macro."
  `(varjo:define-vari-macro ,name ,lambda-list ,@body))

(fl.data:set 'track-dependencies-p nil)
(fl.data:set 'fn->deps (fl.util:dict #'equal))
(fl.data:set 'dep->fns (fl.util:dict #'equal))
(fl.data:set 'stage-fn->programs (fl.util:dict #'equal))
(fl.data:set 'modify-hook (constantly nil))
(reset-program-state)
