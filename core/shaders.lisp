(in-package :%first-light)

(defun initialize-shaders (core)
  (let ((modify-hook (generate-shader-modify-hook core)))
    (setf (shaders core) (fl.gpu:load-shaders modify-hook))))

(defun generate-shader-modify-hook (core)
  (lambda (programs)
    (fl.dst:qpush (recompilation-queue core)
                  (list :shader-recompilation programs))))
