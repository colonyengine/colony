(in-package :first-light.gpu)

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun get-function-spec (function)
  (cons (varjo:name function)
        (mapcar #'second (varjo.internals:in-args function))))

(defun ensure-function-dependency-tables (spec fn-deps dep-fns)
  (unless (au:href dep-fns spec)
    (setf (au:href dep-fns spec) (au:dict #'equal)))
  (setf (au:href fn-deps spec) (au:dict #'equal)))

(defun store-function-dependencies (spec dependencies)
  (symbol-macrolet ((fn-deps (fl.data:get 'fn->deps))
                    (dep-fns (fl.data:get 'dep->fns)))
    (when (au:href fn-deps spec)
      (au:do-hash-keys (k (au:href fn-deps spec))
        (au:when-found (dep-key (au:href dep-fns k))
          (remhash spec dep-key))))
    (ensure-function-dependency-tables spec fn-deps dep-fns)
    (dolist (dep dependencies)
      (let ((dep-spec (get-function-spec dep)))
        (unless (au:href dep-fns dep-spec)
          (setf (au:href dep-fns dep-spec) (au:dict #'equal)))
        (au:do-hash-keys (k (au:href dep-fns spec))
          (setf (au:href fn-deps k dep-spec) dep-spec
                (au:href dep-fns dep-spec k) k))
        (setf (au:href fn-deps spec dep-spec) dep-spec
              (au:href dep-fns dep-spec spec) spec)))))

(defun compute-outdated-programs (spec)
  (let* ((programs)
         (dep->fns (fl.data:get 'dep->fns))
         (spec-fns (au:href dep->fns spec)))
    (maphash
     (lambda (k v)
       (when (or (au:href spec-fns k)
                 (equal k spec))
         (setf programs (union v programs :test #'equal))))
     (fl.data:get 'stage-fn->programs))
    programs))

(defmacro define-function (name args &body body)
  "Define a GPU function."
  (au:with-unique-names (split-details deps fn spec)
    (let ((split-args (varjo.utils:split-arguments args '(&uniform &context))))
      (destructuring-bind (in-args uniforms context) split-args
        `(varjo:with-constant-inject-hook #'lisp-constant->glsl-constant
           (varjo:with-stemcell-infer-hook #'lisp-symbol->glsl-type
             (let* ((,fn (varjo:add-external-function
                          ',name ',in-args ',uniforms ',body))
                    (,spec (get-function-spec ,fn)))
               (when (fl.data:get 'track-dependencies-p)
                 (let* ((,split-details
                          (varjo:test-translate-function-split-details
                           ',name ',in-args ',uniforms ',context ',body))
                        (,deps (varjo:used-external-functions
                                (first ,split-details))))
                   (store-function-dependencies ,spec ,deps)
                   (funcall (fl.data:get 'modify-hook)
                            (compute-outdated-programs ,spec))))
               ,fn))
           (export ',name))))))
