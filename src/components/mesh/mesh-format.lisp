(in-package :first-light)

(defmacro define-mesh (&body body)
  body)

;; It seems we don't actually need the component here, just the location we
;; want to load.
(defun load-mesh (context location)
  (let* ((core-state (core-state context))
         (*package* (find-package (user-package core-state))))
    (with-open-file (in (find-resource core-state location))
      (destructuring-bind (layout buffers) (read in)
        (values buffers (get-vertex-layout core-state layout))))))
