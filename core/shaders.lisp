(in-package :%first-light)

(defmethod extension-file-type ((extension-type (eql :shader-stages)))
  "shd")

(defmethod prepare-extension ((extension-type (eql :shader-stages)) core-state)
  (map-extensions (context core-state) extension-type))

(defmethod extension-file-type ((extension-type (eql :shader-programs)))
  "prog")

(defmethod prepare-extension ((extension-type (eql :shader-programs)) core-state)
  (fl.shaderlib:reset-program-state)
  (map-extensions (context core-state) extension-type)
  (fl.shaderlib:enable-dependency-tracking))

;; NOTE: The returned function is called by the result of doing a C-c, which might be on a different
;; thread due to slynk, or anything else that forced recompilation of shader programs that happened
;; on a different thread. Then in core.flow, in the rendering thread, we dequeue the task and
;; perform the work. This allows us to A) schedule when the recompilation _actually_ happens, and B)
;; not have to worry about different threads making recompilation tasks.
(defun generate-shaders-modified-hook (core-state)
  (lambda (programs-list)
    (v:debug :fl.ext.shader "Shader programs queued for compilation: ~{~s~^, ~}" programs-list)
    (queues:qpush (recompilation-queue core-state) (list :shader-recompilation programs-list))))

(defun recompile-shaders (programs-list)
  (when programs-list
    (fl.shaderlib:translate-shader-programs programs-list)
    (fl.shaderlib:build-shader-programs programs-list)
    (fl.shaderlib:rebind-blocks programs-list)
    (v:debug :fl.ext.shader "Shader programs compiled: ~{~s~^, ~}" programs-list)))

(defun prepare-shader-programs (core-state)
  (setf (shaders core-state) (fl.shaderlib:build-shader-dictionary))
  (fl.shaderlib:set-modify-hook (generate-shaders-modified-hook core-state)))

(defun shutdown-shader-programs ()
  (fl.shaderlib:set-modify-hook (constantly nil))
  (fl.shaderlib:disable-dependency-tracking))

(defmacro define-shader (name (&key (version 430) (primitive :triangles)) &body body)
  `(progn
     (fl.shaderlib:define-shader ,name (:version ,version :primitive ,primitive)
       ,@body)
     (export ',name)))
