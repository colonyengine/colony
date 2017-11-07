(in-package :gear)

(defun map-extensions (extension-type path)
  (map-files
   path
   (lambda (x) (load x :verbose t))
   :filter (extension-type-filter extension-type)))

(defun extension-type-filter (extension-type)
  (lambda (path)
    (string= (pathname-type path)
             (extension-file-type extension-type))))

(defun load-extensions (type path)
  (map-extensions type (get-path :gear "data"))
  (map-extensions type path))

(defun collect-extension-forms (type path)
  (let ((*package* (find-package :gear))
        (results))
    (flet ((%collect (type path)
             (map-files
              path
              (lambda (x)
                (with-open-file (in x)
                  (loop :for form = (read in nil in)
                        :until (eq form in)
                        :for (nil options nil) = form
                        :when (getf options :enabled)
                          :do (push form results))))
              :filter (extension-type-filter type))))
      (%collect type (get-path :gear "data"))
      (%collect type path))
    (nreverse results)))
