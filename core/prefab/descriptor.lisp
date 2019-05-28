(in-package #:first-light.prefab)

(defmacro prefab-descriptor (&body prefab-specs)
  `(list
    ,@(mapcar
       (lambda (x)
         (destructuring-bind (name library &rest args) x
           (let ((library (if (symbolp library) `',library library)))
             `(list ,name ,library ,@args))))
       prefab-specs)))

(defmacro define-prefab-descriptor (name () &body body)
  `(progn
     (unless (fl.data:get 'prefab-descriptors)
       (fl.data:set 'prefab-descriptors (au:dict #'eq)))
     (setf (au:href (fl.data:get 'prefab-descriptors) ',name)
           (prefab-descriptor ,@body))))

(defun find-prefab-descriptor (name)
  (au:href (fl.data:get 'prefab-descriptors) name))
