(in-package #:fl.core)

(defun eql/package-relaxed (obj1 obj2)
  (cond
    ((eql obj1 obj2) t) ; It succeeded? Oh good. Return quickly.
    ((and (symbolp obj1) (symbolp obj2)) ; Otherwise do a slower check.
     (string= (symbol-name obj1)
              (symbol-name obj2)))
    (t ; Hrm, sorry. It didn't EQL match,
     nil)))
