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
