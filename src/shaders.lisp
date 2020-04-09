(in-package #:virality)

(defun initialize-shaders (core)
  (let ((modify-hook (generate-shader-modify-hook)))
    (setf (shaders core) (gpu:load-shaders modify-hook))))

(defun generate-shader-modify-hook ()
  (lambda (x) (push-queue :live-recompile (list :shader x))))
