(in-package :first-light.prefab)

(defun foo (args)
  (mapcar
   (lambda (x)
     (eq (car (au:ensure-list x))))
   args))

(defmacro prefab-descriptor (&body prefab-specs)
  `(list
    ,@(mapcar
       (lambda (x)
         (destructuring-bind (name library &rest args) x
           (let ((library (if (symbolp library) `',library library))
                 (args (loop :for (k v) :on args :by #'cddr
                             :when (and (listp v)
                                        (eq (car v) (print 'static)))
                               :append `(,k ,(cadr v))
                             :else
                               :append `(,k (lambda () ,v)))))
             `(list ,name ,library ,@args))))
       prefab-specs)))

(prefab-descriptor
  ("shrapnel-0" fl.example:examples
                :foo 5
                :ttl (static 3))
  ("shrapnel-1" fl.example:examples
                :ttl (static 1)))
