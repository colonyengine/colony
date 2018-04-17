(in-package :fl.core)

(defun get-extension-path (&optional (system-name :first-light))
  (get-path system-name "data/extension"))

(defun map-extensions (extension-type path &optional owner)
  (map-files
   path
   (lambda (x)
     (let ((package *package*))
       (with-standard-io-syntax
         (let ((*package* package))
           (load x))))
     (simple-logger:emit :extension.load owner x))
   :filter (extension-type-filter extension-type)))

(defun extension-type-filter (extension-type)
  (lambda (path)
    (string= (pathname-type path)
             (extension-file-type extension-type))))

(defun prepare-extensions (core-state path)
  ;; https://github.com/HackerTheory/first-light/wiki/Developer-Rules#extension-order
  (prepare-extension 'settings (context core-state) path)
  (prepare-extension 'graphs core-state path)
  (prepare-extension 'call-flow core-state path)
  (prepare-extension 'shader-stages core-state path)
  (prepare-extension 'shader-programs core-state path)
  (prepare-extension 'materials core-state path)
  (prepare-extension 'scene core-state path))

(defun load-extensions (type path)
  (map-extensions type (get-extension-path) :builtin)
  (map-extensions type path :user))

(defun collect-extension-forms (type path)
  (let ((*package* (find-package :fl.core))
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
      (%collect type (get-extension-path))
      (%collect type path))
    (nreverse results)))
