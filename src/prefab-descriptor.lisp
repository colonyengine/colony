(in-package #:virality.prefabs)

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
     (unless (%fl:meta 'prefab-descriptors)
       (setf (%fl:meta 'prefab-descriptors) (u:dict)))
     (setf (u:href (%fl:meta 'prefab-descriptors) ',name)
           (prefab-descriptor ,@body))))

(defun find-prefab-descriptor (name)
  (u:href (%fl:meta 'prefab-descriptors) name))
