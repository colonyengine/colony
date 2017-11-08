(in-package :gear)

(defmacro prepare-extensions (core-state path)
  `(progn
     ,@(loop :with items = '(context call-flow scene)
             :for item :in items
             :for var = (symbolicate '%temp- item)
             :collect `(let ((,var (make-hash-table :test #'eq)))
                         (declare (special ,var))
                         (flet ((%prepare ()
                                  (load-extensions ',item ,path)
                                  ,var))
                           (maphash
                            (lambda (k v)
                              (setf (gethash k (,(symbolicate item '-table)
                                                ,core-state))
                                    v))
                            (%prepare)))))
     (setf (shaders ,core-state)
           (make-instance 'shaders
                          :data (make-shader-dictionary ,path)))))

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

(defun load-extensions (type path)
  (map-extensions type (get-path :gear "data") :builtin)
  (map-extensions type path :user))

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
