(in-package #:virality)

(defvar *deployed-p*)

(defun deploy-binary (file-name project scene &key compress-p)
  #+sbcl
  (progn
    (setf *deployed-p* t)
    (sb-ext:save-lisp-and-die
     file-name
     :toplevel (lambda () (start :project project :scene scene))
     :executable t
     :compression (when compress-p 9)))
  #-sbcl
  (error "Deployment is only supported on SBCL."))
