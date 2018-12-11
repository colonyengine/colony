(in-package #:%fl)

#+sbcl
(defun deploy-binary (file-name scene-name &key compress-p)
  (v:stop v:*global-controller*)
  (setf uiop/image:*image-dumped-p* t)
  (sb-ext:save-lisp-and-die file-name
                            :toplevel (lambda () (fl:start-engine scene-name))
                            :executable t
                            :compression (when compress-p 9)))

#-sbcl
(defun deploy-binary (file-name scene-name)
  (declare (ignore file-name scene-name))
  (v:warn :fl.core.deploy "Deployment is not supported on this platform."))
