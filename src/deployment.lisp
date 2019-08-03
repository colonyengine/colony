(in-package #:virality.engine)

(defvar *deployed-p*)

(defun deploy-binary (file-name scene &key compress-p)
  #+sbcl
  (progn
    (setf *deployed-p* t)
    (log:stop v:*global-controller*)
    (sb-ext:save-lisp-and-die
     file-name
     :toplevel (lambda () (start-engine :scene scene))
     :executable t
     :compression (when compress-p 9)))
  #-sbcl
  (log:error :virality.engine "Deployment is only supported on SBCL."))
