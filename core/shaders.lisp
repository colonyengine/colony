(in-package :%first-light)

(defun initialize-shaders (core-state)
  (let ((modify-hook (generate-shader-modify-hook core-state)))
    (setf (shaders core-state) (fl.gpu:load-shaders modify-hook))))

(defun generate-shader-modify-hook (core-state)
  (lambda (programs)
    (fl.dst:qpush (recompilation-queue core-state) (list :shader-recompilation programs))))
