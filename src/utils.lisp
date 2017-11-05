(in-package :gear)

(defun get-path (system-name &optional path)
  (if uiop/image:*image-dumped-p*
      (truename
       (uiop/pathname:merge-pathnames*
        path
        (uiop:pathname-directory-pathname (uiop:argv0))))
      (asdf/system:system-relative-pathname (make-keyword system-name) path)))

(defun map-files (path effect &key (filter (constantly t)) (recursivep t))
  (labels ((maybe-affect (file)
             (when (funcall filter file)
               (funcall effect file)))
           (process-files (dir)
             (map nil #'maybe-affect (uiop/filesystem:directory-files dir))))
    (uiop/filesystem:collect-sub*directories
     (uiop/pathname:ensure-directory-pathname path)
     t recursivep #'process-files)))
