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
  (map-extensions owner (get-path :gear "data"))
  (map-extensions owner path))

(defun collect-extension-forms (owner path)
  (let ((*package* (find-package :gear))
        (results))
    (flet ((%collect (owner path)
             (map-files
              path
              (lambda (x)
                (with-open-file (in x)
                  (loop :for form = (read in nil in)
                        :until (eq form in)
                        :do (push form results))))
              :filter (extension-type-filter owner))))
      (%collect owner (get-path :gear "data"))
      (%collect owner path))
    (nreverse results)))
