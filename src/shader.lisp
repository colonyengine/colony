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

(defun parse-shader-dictionary (name version spec)
  `(kit.gl.shader:defdict ,name (:uniform-style :camel-case)
     ,@(loop :for (stages programs) :on spec :by #'cddr
             :append (%generate-stages version stages)
             :append (%generate-programs programs))))

(defmethod extension-file-types ((owner (eql 'shader)))
  (list "shd"))

(defmacro shader-dictionary (name (&key enabled version) &body body)
  `(let ()
     (declare (special %temp-shader))
     ,(when enabled
        `(setf (gethash ,name %temp-shader)
               ,(parse-shader-dictionary name version body)))))
