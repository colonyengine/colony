(in-package #:%first-light)

;; I designed the system such that the direction the annotation goes is the same
;; for reading and writing.
(defun %material-annotator (mat-val component)
  (with-accessors ((context context)) component
    (typecase mat-val
      (symbol
       (lookup-material mat-val context))
      (cons
       (destructuring-bind (base-mat-sym new-mat-sym
                            &key shader instances uniforms blocks)
           mat-val
         (let* ((base-mat (lookup-material base-mat-sym context))
                (copy-mat (copy-material base-mat new-mat-sym)))
           (when blocks
             (error "Material override: :blocks not implemented yet."))
           ;; First, change to the new shader
           (when shader
             (setf (shader copy-mat) shader))
           (when instances
             (setf (instances copy-mat) instances))
           ;; Then process the initargs for the new shader.
           (when uniforms
             (unless (every (lambda (x) (= (length x) 2)) uniforms)
               (error "Material override: :uniforms entries must have a length ~
                       of 2 ~A~%"
                      uniforms))
             (loop :for (uniform-name value) :in uniforms
                   :do (setf (mat-uniform-ref copy-mat uniform-name) value)))
           ;; and return the newly minted material with all the overrides.
           copy-mat)))
      (t
       mat-val))))

(defmacro define-annotation (name &key
                                    (getter
                                     '(lambda (value component)
                                       (declare (ignore component)
                                        value)))
                                    (setter
                                     '(lambda (value component)
                                       (declare (ignore component)
                                        value))))
  `(register-annotation 'component ',name :initialized
                        :getter (function ,getter)
                        :setter (function ,setter)))
