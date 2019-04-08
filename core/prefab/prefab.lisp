(in-package :first-light.prefab)

(defmacro preprocess-spec (name context spec)
  (labels ((rec (data)
             (au:mvlet ((name components children (split-spec data)))
               `(list ',name
                      ,@(mapcar #'thunk components)
                      ,@(mapcar #'rec children))))
           (thunk (data)
             (destructuring-bind (type . options/args) data
               (let ((options-p (listp (first options/args))))
                 `(list ',type
                        ',(when options-p (first options/args))
                        ,@(loop :with args = (if options-p
                                                 (rest options/args)
                                                 options/args)
                                :for (key value) :on args :by #'cddr
                                :collect key
                                :collect `(lambda (,context)
                                            (declare (ignorable ,context))
                                            ,value)))))))
    `(values
      (list ,@(mapcar #'rec (list (cons name spec))))
      (au:dlambda
        (:actors (x) (setf actor-table x))
        (:components (x) (setf component-table x))
        (:current-actor (x) (setf current-actor x))))))

(defmacro inject-ref-environment (&body body)
  `(let (actor-table component-table current-actor)
     (flet ((ref (&rest args)
              (lookup-reference
               args current-actor actor-table component-table)))
       ,@body)))

(defmacro define-prefab (name (&key library (context 'context)) &body body)
  (let* ((libraries '(fl.data:get 'prefabs))
         (prefabs `(au:href ,libraries ',library)))
    (au:with-unique-names (prefab data setter)
      `(progn
         (ensure-prefab-name-string ',name)
         (ensure-prefab-name-valid ',name)
         (ensure-prefab-library-set ',name ',library)
         (ensure-prefab-library-symbol ',name ',library)
         (unless ,libraries
           (fl.data:set 'prefabs (au:dict #'eq)))
         (unless ,prefabs
           (setf ,prefabs (au:dict #'equalp)))
         (inject-ref-environment
           (au:mvlet* ((,data ,setter (preprocess-spec ,name ,context ,body))
                       (,prefab (make-prefab ',name ',library ,data)))
             (setf (au:href ,prefabs ',name) ,prefab
                   (func ,prefab) (make-factory ,prefab ,setter))
             (parse-prefab ,prefab)))
         (export ',library)))))
