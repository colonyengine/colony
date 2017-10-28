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

(defgeneric load-extensions (owner path)
  (:method-combination progn :most-specific-last)
  (:method progn (owner path)
    (map-extensions owner (get-path :gear "extensions"))))
