(in-package #:colony)

(defmacro define-physics-config (name options &body body)
  ;; NOTE: We convert the DSL into a lambda form at macro
  ;; expansion time that then gets compiled into a thunk that we
  ;; execute at engine start time to create the actual
  ;; physics-config instance..
  (let ((physics-config-constructor
          (parse-physics-config name options body)))
    `(progn
       (symbol-macrolet ((desc-lookup
                           (u:href c::=meta/physics-configs= ',name)))
         (let ((new-desc (make-physics-config-descriptor
                          ',name
                          `(define-physics-config ,',name ,',options
                             ,@',body)
                          ',physics-config-constructor
                          ,physics-config-constructor
                          ))
               (old-desc desc-lookup))
           (setf desc-lookup new-desc)
           (update-physics-config/interactively old-desc new-desc)
           (values
            (name new-desc)
            (constructor new-desc)))))))
