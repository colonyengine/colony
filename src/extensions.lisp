(in-package :fl.core)

(defun get-extension-path (&optional (system-name :first-light))
  (au:resolve-system-path system-name "data/"))

(defun map-extensions (extension-type path &optional owner)
  (au:map-files
   path
   (lambda (x)
     (let ((package *package*))
       (with-standard-io-syntax
         (let ((*package* package)
               (*print-readably* nil))
           (load x))))
     (simple-logger:emit :extension.load owner x))
   :test (extension-type-filter extension-type)))

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

(defun prepare-extensions (core-state path)
  ;; https://github.com/HackerTheory/first-light/wiki/Developer-Rules#extension-order
  (let ((*context* (context core-state)))
    (prepare-extension :host core-state path)
    (prepare-extension :graphs core-state path)
    (prepare-extension :call-flow core-state path)
    (prepare-extension :shader-stages core-state path)
    (prepare-extension :shader-programs core-state path)
    (prepare-extension :textures core-state path)
    (prepare-extension :materials core-state path)
    (prepare-extension :scene core-state path)))

(defun load-extensions (type path)
  (map-extensions type (get-extension-path) :builtin)
  (map-extensions type path :user))

(defun collect-extension-forms (type path)
  (let ((*package* (find-package :fl.core))
        (results))
    (flet ((%collect (type path)
             (au:map-files
              path
              (lambda (x)
                (au:with-file-input (in x)
                  (loop :for form = (read in nil in)
                        :until (eq form in)
                        :for (nil options nil) = form
                        :do (push form results))))
              :test (extension-type-filter type))))
      (%collect type (get-extension-path))
      (%collect type path))
    (nreverse results)))
