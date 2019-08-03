(in-package #:virality.prefabs)

(defmacro prefab-descriptor (&body prefab-specs)
  `(list
    ,@(mapcar
       (lambda (x)
         (destructuring-bind (name library . args) x
           (let ((library (if (symbolp library) `',library library)))
             `(list ,name ,library ,@args))))
       prefab-specs)))

(defmacro define-prefab-descriptor (name () &body body)
  `(progn
     (unless (v::meta 'prefab-descriptors)
       (setf (v::meta 'prefab-descriptors) (u:dict)))
     (setf (u:href (v::meta 'prefab-descriptors) ',name)
           (prefab-descriptor ,@body))))

(defun find-prefab-descriptor (name)
  (u:href (v::meta 'prefab-descriptors) name))
