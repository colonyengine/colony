(in-package :gear)

(defmethod kit.gl.shader:parse-shader-source-complex
    ((key (eql :generate)) params shader-type shader-list)
  (destructuring-bind (version stage function) params
    (3bgl-shaders:generate-stage stage function :version version)))

(defun %type-check-shader-symbol (symbol)
  (or (find-symbol (symbol-name symbol) :gear-shaders)
      (error "Shader stage ~a not defined in the :GEAR-SHADERS package."
             symbol)))

(defun %generate-stage-symbol (stage)
  (make-keyword (symbolicate stage '-shader)))

(defun %generate-stages (version data)
  (loop :for (stage func) :in data
        :collect `(shader ,func
                          ,(%generate-stage-symbol stage)
                          (:generate ,version
                                     ,stage
                                     ,(%type-check-shader-symbol func)))))

(defun %generate-uniform-symbol (parts)
  (make-keyword
   (with-output-to-string (s)
     (loop :for (part . rest) :on parts
           :do (etypecase part
                 (number (format s "[~a]" part))
                 (symbol (format s "~a" part)))
           :when rest
             :do (format s ".")))))

(defun %generate-uniform-list (&rest stages)
  (remove-duplicates
   (loop :for (stage func) :on stages :by #'cddr
         :for uniforms = (nth-value 1 (3bgl-shaders:generate-stage
                                       stage
                                       (%type-check-shader-symbol func)
                                       :expand-uniforms t))
         :append (loop :for (lisp nil nil . properties) :in uniforms
                       :for components = (getf properties :components)
                       :append (mapcar #'%generate-uniform-symbol components)))))

(defun %generate-program-stages (data)
  (loop :for (stage func) :on data :by #'cddr
        :collect (list (%generate-stage-symbol stage)
                       (%type-check-shader-symbol func))))

(defun %generate-programs (data)
  (loop :for (name stages) :in data
        :collect `(program ,name
                           ,(apply #'%generate-uniform-list stages)
                           ,@(%generate-program-stages stages))))

(defmethod extension-file-types ((owner (eql 'shader)))
  (list "shd"))

(defmacro %with-shader-forms ((forms) &body body)
  `(dolist (form ,forms)
     (destructuring-bind (type options data) form
       (declare (ignorable type options data))
       ,@body)))

(defmacro %shader-symbol-duplicate-check ((data key) &body body)
  `(find-if
    (lambda (x)
      (when (> (count x ,data :key ,key) 1)
        ,@body))
    ,data
    :key ,key))

(defun %type-check-stages (forms)
  (let ((fn-symbols))
    (%with-shader-forms (forms)
      (loop :for (nil . fns) :in data
            :do (appendf fn-symbols fns))
      (%shader-symbol-duplicate-check (data #'car)
        (error "Stage ~s cannot be defined more than once in the same ~s form."
               x type))
      (%shader-symbol-duplicate-check (fn-symbols #'identity)
        (error "Function ~s cannot be defined more than once in all ~s forms."
               x type)))))

(defun %type-check-programs (forms)
  (let ((program-names))
    (%with-shader-forms (forms)
      (loop :for (name  nil) :in data
            :do (push name program-names))
      (%shader-symbol-duplicate-check (program-names #'identity)
        (error "Program ~s must not be defined more than once in all ~s forms."
               x type))
      (flet ((%check (program stages)
               (when (> (length stages)
                        (length (remove-duplicates stages)))
                 (error "Program ~s must not have multiple of the same stage."
                        (car program)))))
        (dolist (program data)
          (loop :for (key value) :on (cadr program) :by #'cddr
                :collect key :into stages
                :finally (%check program stages)))))))

(defun %collect-shader-stages (forms)
  (flet ((%collect ()
           (loop :for (nil nil . stages) :in forms
                 :append (loop :for (stage fn) :in stages
                               :append (list stage fn)))))
    (loop :with table = (make-hash-table)
          :for (stage . fns) :in (%collect)
          :do (appendf (gethash stage table) fns)
          :finally (return (nreverse (hash-table-alist table))))))

(defun %collect-shader-programs (forms)
  (let ((programs-list))
    (%with-shader-forms (forms)
      (appendf programs-list data))
    programs-list))

(defun %collect-shader-forms (path)
  (let ((all-forms (collect-extension-forms 'shader path))
        (stage-forms)
        (program-forms))
    (mapcar
     (lambda (x)
       (destructuring-bind (type options . rest) x
         (declare (ignore rest))
         (when (getf options :enabled)
           (case type
             (shader-stages (appendf stage-forms (list x)))
             (shader-programs (appendf program-forms (list x)))))))
     all-forms)
    (%type-check-stages stage-forms)
    (%type-check-programs program-forms)
    (values (%collect-shader-stages stage-forms)
            (%collect-shader-programs program-forms))))

(defun make-shader-dictionary (extension-path)
  (multiple-value-bind (stages programs) (%collect-shader-forms extension-path)
    (kit.gl.shader:define-dictionary :shaders
        (loop :for (type name uniforms . stages) :in (%generate-programs programs)
              :collect (make-instance 'kit.gl.shader::program-source
                                      :name name
                                      :uniform-style :camel-case
                                      :uniforms uniforms
                                      :shaders stages))
      :shaders (mapcar #'rest (%generate-stages 330 stages)))))
