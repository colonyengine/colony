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
  (au:with-unique-names (setter actors)
    `(let ((,actors (au:dict)))
       (flet ((,setter (arg)
                (setf ,actors arg))
              (ref (actor &optional ctype)
                (declare (ignore ctype))
                (let ((actor (au:href ,actors actor)))
                  (values actor ,actors))))
         (values
          (thunk-spec ,context ,spec)
          #',setter)))))

(defmacro define-prefab (name (&key library (context 'context)) &body body)
  (let* ((libraries '(fl.data:get 'prefabs))
         (prefabs `(au:href ,libraries ',library))
         (body (list (cons name body))))
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
         (au:mvlet* ((,data ,setter (preprocess-spec ,context ,body))
                     (,prefab (make-prefab ',name ',library ,data)))
           (setf (au:href ,prefabs ',name) ,prefab
                 (slot-value ,prefab '%func) (make-factory ,prefab ,setter))
           (parse-prefab ,prefab))
         (export ',library)))))
