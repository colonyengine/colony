(in-package :%fl)

(defun map-extensions (context extension-type)
  (flet ((%map (type path)
           (fu:map-files
            path
            (lambda (x)
              (with-standard-io-syntax
                (let ((*package* (find-package :%fl))
                      (*print-readably* nil))
                  (load x))))
            :test (extension-type-filter type))))
    (%map extension-type (find-resource context '(:core :ext)))
    (%map extension-type (find-resource context :ext))))

(defun extension-type-filter (extension-type)
  (lambda (path)
    (let ((name (namestring (pathname-name path))))
      (and (string= (pathname-type path)
                    (extension-file-type extension-type))
           ;; NOTE: Ignore emacs lockfiles.
           ;;
           ;; TODO: Probably should use real regexes for this and have a config
           ;; file entry for all the stuff we can ignore.
           (if (>= (length name) 2)
               (not (string= ".#" name :end2 2))
               t)))))

(defun collect-extension-forms (context type)
  (let ((*package* (find-package :%fl))
        (results))
    (flet ((%collect (type path)
             (fu:map-files
              path
              (lambda (x)
                (fu:with-file-input (in x)
                  (loop :for form = (read in nil in)
                        :until (eq form in)
                        :for (nil options nil) = form
                        :do (push form results))))
              :test (extension-type-filter type))))
      (%collect type (find-resource context '(:core :ext)))
      (%collect type (find-resource context :ext)))
    (nreverse results)))
