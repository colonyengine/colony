(in-package :gear)

(defun get-path (system &optional path)
  (if uiop/image:*image-dumped-p*
      (truename
       (uiop/pathname:merge-pathnames*
        path
        (uiop:pathname-directory-pathname (uiop:argv0))))
      (asdf/system:system-relative-pathname system path)))

(defun map-files (path effect &key (filter (constantly t)) (recursivep t))
  (labels ((maybe-affect (file)
             (when (funcall filter file)
               (funcall effect file)))
           (process-files (dir)
             (map nil #'maybe-affect (uiop/filesystem:directory-files dir))))
    (uiop/filesystem:collect-sub*directories
     (uiop/pathname:ensure-directory-pathname path)
     t recursivep #'process-files)))

(defun pathname-type-filter (&rest strings)
  "Return a single argument function that checks if the PATHANME-TYPE
of that argument is one of the STRINGS. Return T if true, NIL otherwise."
  (lambda (path)
    (some (lambda (str)
            (string= (pathname-type path) str))
          strings)))

(defun load-extensions (path)
  (map-files
   path
   (lambda (x) (load x :verbose t))
   :filter (pathname-type-filter "lisp" "scene")))
