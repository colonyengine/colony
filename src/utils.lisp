(in-package :gear)

(defun get-path (system &optional path)
  "Get the absolute path of a resource relative to the system designated by
SYSTEM. This is needed to refer to file resources on disk, in a method that
works for both interactive development and dumped core images where system paths
would otherwise refer to those on the host machine."
  (if uiop/image:*image-dumped-p*
      (truename
       (uiop/pathname:merge-pathnames*
        path
        (uiop:pathname-directory-pathname (uiop:argv0))))
      (asdf/system:system-relative-pathname system path)))

(defun map-files (path effect &key (filter (constantly t)) (recursive t))
  (labels ((maybe-affect (file)
             (when (funcall filter file)
               (funcall effect file)))
           (process-files (dir)
             (map nil #'maybe-affect (uiop/filesystem:directory-files dir))))
    (uiop/filesystem:collect-sub*directories
     (uiop/pathname:ensure-directory-pathname path)
     t recursive #'process-files)))

(defun load-extensions ()
  (map-files
   path
   (lambda (x) (load x :verbose t))
   :filter (lambda (x) (or (string= (pathname-type x) "lisp")
                      (string= (pathname-type x) "scene")))))
