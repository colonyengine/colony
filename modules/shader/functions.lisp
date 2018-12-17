(in-package :first-light.shader)

(cl:defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(cl:defun get-function-spec (function)
  (cons (varjo:name function) (mapcar #'second (varjo.internals:in-args function))))

(cl:defun ensure-function-dependency-tables (spec fn-deps dep-fns)
  (unless (fl.util:href dep-fns spec)
    (setf (fl.util:href dep-fns spec) (fl.util:dict #'equal)))
  (setf (fl.util:href fn-deps spec) (fl.util:dict #'equal)))

(cl:defun store-function-dependencies (spec dependencies)
  (symbol-macrolet ((fn-deps (fl.data:get 'fn->deps))
                    (dep-fns (fl.data:get 'dep->fns)))
    (when (fl.util:href fn-deps spec)
      (fl.util:do-hash-keys (k (fl.util:href fn-deps spec))
        (fl.util:when-found (dep-key (fl.util:href dep-fns k))
          (remhash spec dep-key))))
    (ensure-function-dependency-tables spec fn-deps dep-fns)
    (dolist (dep dependencies)
      (let ((dep-spec (get-function-spec dep)))
        (unless (fl.util:href dep-fns dep-spec)
          (setf (fl.util:href dep-fns dep-spec) (fl.util:dict #'equal)))
        (fl.util:do-hash-keys (k (fl.util:href dep-fns spec))
          (setf (fl.util:href fn-deps k dep-spec) dep-spec
                (fl.util:href dep-fns dep-spec k) k))
        (setf (fl.util:href fn-deps spec dep-spec) dep-spec
              (fl.util:href dep-fns dep-spec spec) spec)))))

(cl:defun compute-outdated-programs (spec)
  (let* ((programs)
         (dep->fns (fl.data:get 'dep->fns))
         (spec-fns (fl.util:href dep->fns spec)))
    (maphash
     (lambda (k v)
       (when (or (fl.util:href spec-fns k)
                 (equal k spec))
         (setf programs (union v programs :test #'equal))))
     (fl.data:get 'stage-fn->programs))
    programs))

(cl:defmacro defun (name args &body body)
  "Define a GPU function."
  (fl.util:with-unique-names (split-details deps fn spec)
    (let ((split-args (varjo.utils:split-arguments args '(&uniform &context))))
      (destructuring-bind (in-args uniforms context) split-args
        `(varjo:with-constant-inject-hook #'lisp-constant->glsl-constant
           (varjo:with-stemcell-infer-hook #'lisp-symbol->glsl-type
             (let* ((,fn (varjo:add-external-function ',name ',in-args ',uniforms ',body))
                    (,spec (get-function-spec ,fn)))
               (when (fl.data:get 'track-dependencies-p)
                 (let* ((,split-details (varjo:test-translate-function-split-details
                                         ',name ',in-args ',uniforms ',context ',body))
                        (,deps (varjo:used-external-functions (first ,split-details))))
                   (store-function-dependencies ,spec ,deps)
                   (funcall (fl.data:get 'modify-hook) (compute-outdated-programs ,spec))))
               ,fn))
           (export ',name))))))
