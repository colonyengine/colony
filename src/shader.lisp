(in-package :gear)

(defclass shaders ()
  ((%data :reader data
          :initarg :data)
   (%compiled-shaders :accessor compiled-shaders)
   (%modified-functions :accessor modified-functions
                        :initform nil)))

(defmacro shader-stages (&body body)
  body)

(defmacro shader-programs (&body body)
  body)

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

(defun %type-check-shader-function (symbol)
  (or (find-symbol (symbol-name symbol) :gear-shaders)
      (error "Function ~a not defined in the :GEAR-SHADERS package." symbol)))

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
      (loop :for (name . nil) :in data
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

(defun %generate-uniforms (&rest stages)
  (flet ((%generate-uniform-symbol (parts)
           (make-keyword
            (with-output-to-string (s)
              (loop :for (part . rest) :on parts
                    :do (etypecase part
                          (number (format s "[~a]" part))
                          (symbol (format s "~a" part)))
                    :when rest
                      :do (format s "."))))))
    (remove-duplicates
     (loop :for (stage func) :on stages :by #'cddr
           :for uniforms = (nth-value 1 (3bgl-shaders:generate-stage
                                         stage
                                         (%type-check-shader-function func)
                                         :expand-uniforms t))
           :append (loop :for (lisp nil nil . properties) :in uniforms
                         :for components = (getf properties :components)
                         :append (mapcar #'%generate-uniform-symbol
                                         components))))))

(defun %make-dictionary-programs (data)
  (flet ((%generate-stages (stages)
           (loop :for (stage func) :on stages :by #'cddr
                 :collect `(,(make-keyword (symbolicate stage '-shader))
                            ,(%type-check-shader-function func)))))
    (loop :for (name stages) :in data
          :collect `(:name ,name
                     :uniforms ,(apply #'%generate-uniforms stages)
                     :shaders ,(%generate-stages stages)))))

(defun %make-dictionary-stages (data)
  (loop :for (type . rest) :in data
        :append (loop :for item :in rest
                      :for (func . options) = item
                      :for func-symbol = (%type-check-shader-function func)
                      :collect
                      `(,func-symbol
                        ,(make-keyword (symbolicate type '-shader))
                        (:generate ,(getf options :version)
                                   ,type
                                   ,func-symbol)))))

(defun %process-stage-forms (forms)
  (let ((result))
    (loop :for (nil options . (stages)) :in forms
          :for default-version = (getf options :default-version 330)
          :do (loop :for (type . funcs) :in stages
                    :do (loop :for func :in funcs
                              :for options = (cdr (ensure-list func))
                              :unless (getf options :version)
                                :do (setf func (append
                                                (list func)
                                                `(:version ,default-version)))
                              :do (push (list type func) result))))
    (loop :with table = (make-hash-table)
          :for (type func) :in result
          :do (push func (gethash type table))
          :finally (return (nreverse (hash-table-alist table))))))

(defun %process-program-forms (forms)
  (let ((programs-list))
    (%with-shader-forms (forms)
      (appendf programs-list data))
    programs-list))

(defun %collect-shader-forms (path)
  (loop :for form :in (collect-extension-forms 'shader path)
        :for (form-type . nil) = form
        :when (eq form-type 'shader-stages)
          :collect form :into stages
        :when (eq form-type 'shader-programs)
          :collect form :into programs
        :finally (%type-check-stages stages)
                 (%type-check-programs programs)
                 (return (values (%process-stage-forms stages)
                                 (%process-program-forms programs)))))

(defmethod kit.gl.shader:parse-shader-source-complex
    ((key (eql :generate)) params shader-type shader-list)
  (destructuring-bind (version stage function) params
    (3bgl-shaders:generate-stage stage function :version version)))

(defmethod extension-file-type ((extension-type (eql 'shader)))
  "shd")

(defun make-shader-dictionary (extension-path)
  (multiple-value-bind (stages programs) (%collect-shader-forms extension-path)
    (kit.gl.shader:define-dictionary :shaders
        (loop :for program :in (%make-dictionary-programs programs)
              :collect (apply #'make-instance 'kit.gl.shader::program-source
                              :uniform-style :camel-case
                              program))
      :shaders (%make-dictionary-stages stages))))

(defun shaders-modified-hook-generator (core-state)
  (lambda (x)
    (setf (modified-functions (shaders core-state))
          (union x (modified-functions (shaders core-state))))))

(defun compile-shaders (core-state)
  (setf (compiled-shaders (shaders core-state))
        (kit.gl.shader:compile-shader-dictionary :shaders))
  (pushnew (shaders-modified-hook-generator core-state)
           3bgl-shaders:*modified-function-hook*)
  (dolist (func (modified-functions (shaders core-state)))
    (slog:emit :shader.function.compiled func)))

(defun maybe-recompile-shaders (core-state)
  (when (modified-functions (shaders core-state))
    (compile-shaders core-state)
    (setf (modified-functions (shaders core-state)) nil)))
