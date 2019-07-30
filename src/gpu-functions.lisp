(in-package #:first-light.gpu)

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun get-function-spec (function)
  (cons (varjo:name function)
        (mapcar #'second (varjo.internals:in-args function))))

(defun ensure-function-dependency-tables (spec fn-deps dep-fns)
  (unless (u:href dep-fns spec)
    (setf (u:href dep-fns spec) (u:dict #'equal)))
  (setf (u:href fn-deps spec) (u:dict #'equal)))

(defun store-function-dependencies (spec dependencies)
  (symbol-macrolet ((fn-deps (%fl:meta 'fn->deps))
                    (dep-fns (%fl:meta 'dep->fns)))
    (when (u:href fn-deps spec)
      (u:do-hash-keys (k (u:href fn-deps spec))
        (u:when-found (dep-key (u:href dep-fns k))
          (remhash spec dep-key))))
    (ensure-function-dependency-tables spec fn-deps dep-fns)
    (dolist (dep dependencies)
      (let ((dep-spec (get-function-spec dep)))
        (unless (u:href dep-fns dep-spec)
          (setf (u:href dep-fns dep-spec) (u:dict #'equal)))
        (u:do-hash-keys (k (u:href dep-fns spec))
          (setf (u:href fn-deps k dep-spec) dep-spec
                (u:href dep-fns dep-spec k) k))
        (setf (u:href fn-deps spec dep-spec) dep-spec
              (u:href dep-fns dep-spec spec) spec)))))

(defun compute-outdated-programs (spec)
  (let* ((programs)
         (dep->fns (%fl:meta 'dep->fns))
         (spec-fns (u:href dep->fns spec)))
    (maphash
     (lambda (k v)
       (when (or (u:href spec-fns k)
                 (equal k spec))
         (setf programs (union v programs :test #'equal))))
     (%fl:meta 'stage-fn->programs))
    programs))

(defmacro define-function (name args &body body)
  "Define a GPU function."
  (a:with-gensyms (split-details deps fn spec)
    (let ((split-args (varjo.utils:split-arguments args '(&uniform &context))))
      (destructuring-bind (in-args uniforms context) split-args
        `(varjo:with-constant-inject-hook #'lisp-constant->glsl-constant
           (varjo:with-stemcell-infer-hook #'lisp-symbol->glsl-type
             (let* ((,fn (varjo:add-external-function
                          ',name ',in-args ',uniforms ',body))
                    (,spec (get-function-spec ,fn)))
               (when (%fl:meta 'track-dependencies-p)
                 (let* ((,split-details
                          (varjo:test-translate-function-split-details
                           ',name ',in-args ',uniforms ',context ',body))
                        (,deps (varjo:used-external-functions
                                (first ,split-details))))
                   (store-function-dependencies ,spec ,deps)
                   (funcall (%fl:meta 'modify-hook)
                            (compute-outdated-programs ,spec))))
               ,fn))
           (export ',name))))))
