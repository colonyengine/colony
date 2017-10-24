(in-package :gear)

(defun get-path (system &optional path)
  "Get the absolute path of a resource relative to the system designated by
SYSTEM. This is needed to accurated refer to resources, such as a scene
definition file, when the path to the system may be different than what it was
before dumping an image. Thus, this function will return the correct full path
to a file or sub-directory regardless of whether it is called interactively, or
from a deployed binary image which would otherwise return a path to the system
as it was before image dumping occured."
  (if uiop/image:*image-dumped-p*
      (truename
       (uiop/pathname:merge-pathnames*
        path
        (uiop:pathname-directory-pathname (uiop:argv0))))
      (asdf/system:system-relative-pathname system path)))
