(in-package #:%fl)

#-(or sbcl)
(defun deploy-binary (file-name scene-name)
  (declare (ignore file-name scene-name))
  (format t "DEPLOY-BINARY is not supported on this platform."))

#+sbcl
(defun deploy-binary (file-name scene-name)
  (flet ((init ()
           (fl:start-engine scene-name)))
    ;; needed for golden-utils to find binary-relative assets
    (setf uiop/image:*image-dumped-p* t)
    (sb-ext:save-lisp-and-die
     file-name
     :toplevel #'init
     :executable t)))
