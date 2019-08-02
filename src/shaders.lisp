(in-package #:%first-light)

(defun initialize-shaders (core)
  (let ((modify-hook (generate-shader-modify-hook core)))
    (setf (shaders core) (gpu:load-shaders modify-hook))))

(defun generate-shader-modify-hook (core)
  (lambda (programs)
    (queues:qpush (recompilation-queue core)
                  (list :shader-recompilation programs))))
