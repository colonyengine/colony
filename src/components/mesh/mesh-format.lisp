(in-package :first-light)

(defmacro define-mesh (&body body)
  body)

(defun load-mesh (context location)
  (let* ((core-state (core-state context))
         (*package* (find-package (user-package core-state))))
    (with-open-file (in (find-resource core-state location))
      (destructuring-bind (layout-id buffers) (read in)
        (values (get-vertex-layout core-state layout-id)
                buffers)))))
