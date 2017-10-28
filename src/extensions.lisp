(in-package :gear)

(defun map-extensions (owner path)
  (map-files
   path
   (lambda (x) (load x :verbose t))
   :filter (extension-type-filter owner)))

(defun extension-type-filter (owner)
  (lambda (path)
    (some (lambda (str)
            (string= (pathname-type path) str))
          (extension-file-types owner))))

(defun load-extensions (owner path)
  (map-extensions owner (get-path :gear "extensions"))
  (map-extensions owner path))
