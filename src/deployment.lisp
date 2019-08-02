(in-package #:virality.engine)

#+sbcl
(defun deploy-binary (file-name scene &key compress-p)
  (log:stop v:*global-controller*)
  (setf uiop/image:*image-dumped-p* t)
  (sb-ext:save-lisp-and-die
   file-name
   :toplevel (lambda () (start-engine :scene scene))
   :executable t
   :compression (when compress-p 9)))

#-sbcl
(defun deploy-binary (file-name scene-name)
  (declare (ignore file-name scene-name))
  (log:warn :changeme "Deployment is not supported on this platform."))
