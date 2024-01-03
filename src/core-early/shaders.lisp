(in-package #:virality)

(defun initialize-shaders (core)
  (let ((modify-hook (generate-shader-modify-hook)))
    (setf (shaders core) (shadow:load-shaders modify-hook))))

(defun generate-shader-modify-hook ()
  (lambda (x)
    (v:with-selected-interactive-core (core)
      (tpool:push-queue (thread-pool core)
                        :recompile (list :shader x)))))
