(in-package :first-light)

(defun map-extensions (extension-type path &optional owner)
  (map-files
   path
   (lambda (x)
     (load x)
     (slog:emit :extension.load owner x))
   :filter (extension-type-filter extension-type)))

(defun extension-type-filter (extension-type)
  (lambda (path)
    (string= (pathname-type path)
             (extension-file-type extension-type))))

(defun prepare-extensions (core-state path)
  (prepare-extension 'settings (context core-state) path)
  (prepare-extension 'call-flow core-state path)
  (prepare-extension 'scene core-state path)
  (prepare-extension 'shader core-state path)
  (prepare-extension 'vertex core-state path)
  (prepare-extension 'graphs core-state path))

(defun load-extensions (type path)
  (map-extensions type (get-path :first-light "data") :builtin)
  (map-extensions type path :user))

(defun collect-extension-forms (type path)
  (let ((*package* (find-package :first-light))
        (results))
    (flet ((%collect (type path)
             (map-files
              path
              (lambda (x)
                (with-open-file (in x)
                  (loop :for form = (read in nil in)
                        :until (eq form in)
                        :for (nil options nil) = form
                        :do (push form results))))
              :filter (extension-type-filter type))))
      (%collect type (get-path :first-light "data"))
      (%collect type path))
    (nreverse results)))
