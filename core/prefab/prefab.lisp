(in-package :first-light.prefab)

(defun thunk-component-args (context data)
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
                                 ,value))))))

(defmacro thunk-spec (context spec)
  (labels ((traverse-children (data)
             (au:mvlet ((name components children (split-spec data)))
               `(list ',name
                      ,@(mapcar
                         (lambda (x) (thunk-component-args context x))
                         components)
                      ,@(mapcar #'traverse-children children)))))
    `(list ,@(mapcar #'traverse-children spec))))

(defmacro preprocess-spec (context spec)
  (au:with-unique-names
      (actor-table-setter actor-table current-actor-setter current)
    `(let ((,actor-table (au:dict))
           (,current))
       (flet ((,actor-table-setter (value)
                (setf ,actor-table value))
              (,current-actor-setter (value)
                (setf ,current value))
              (ref (id &key component merge-id)
                (let ((reference (make-reference
                                  id
                                  ,current
                                  ,actor-table
                                  component
                                  merge-id)))
                  (parse-reference reference))))
         (values
          (thunk-spec ,context ,spec)
          #',actor-table-setter
          #',current-actor-setter)))))

(defmacro define-prefab (name (&key library (context 'context)) &body body)
  (let* ((libraries '(fl.data:get 'prefabs))
         (prefabs `(au:href ,libraries ',library))
         (body (list (cons name body))))
    (au:with-unique-names (prefab data actor-table-setter current-actor-setter)
      `(progn
         (ensure-prefab-name-string ',name)
         (ensure-prefab-name-valid ',name)
         (ensure-prefab-library-set ',name ',library)
         (ensure-prefab-library-symbol ',name ',library)
         (unless ,libraries
           (fl.data:set 'prefabs (au:dict #'eq)))
         (unless ,prefabs
           (setf ,prefabs (au:dict #'equalp)))
         (au:mvlet* ((,data ,actor-table-setter ,current-actor-setter
                            (preprocess-spec ,context ,body))
                     (,prefab (make-prefab ',name ',library ,data)))
           (setf (au:href ,prefabs ',name) ,prefab
                 (slot-value ,prefab '%func) (make-factory
                                              ,prefab
                                              ,actor-table-setter
                                              ,current-actor-setter))
           (parse-prefab ,prefab))
         (export ',library)))))
