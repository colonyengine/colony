(in-package #:virality.engine)

(defun initialize-shaders (core)
  (let ((modify-hook (generate-shader-modify-hook core)))
    (setf (shaders core) (gpu:load-shaders modify-hook))))

(defun generate-shader-modify-hook (core)
  (lambda (x) (push-queue core :recompile-shader x)))
