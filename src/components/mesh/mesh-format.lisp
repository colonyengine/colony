(in-package :first-light)

(defmacro define-mesh (&body body)
  body)

(defun load-mesh (context component)
  (let* ((core-state (core-state context))
         (*package* (find-package (user-package core-state))))
    (with-open-file (in (find-resource core-state (location component)))
      (destructuring-bind (layout buffers) (read in)
        (setf (layout component) (get-vertex-layout core-state layout))
        buffers))))
