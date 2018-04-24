(in-package :fl.core)

(defgeneric destroy (thing context &key ttl)
  (:documentation "Destroy may take either an ACTOR or a COMPONENT. The keyword argument :TTL
supplied in real seconds, how long the thing has yet to live."))

(defun type-table (key type-table)
  (au:href type-table key))

(defun (setf type-table) (entry type-name-key type-table)
  (symbol-macrolet ((entry-table (au:href type-table type-name-key)))
    (unless (nth-value 1 entry-table)
      (setf entry-table (au:dict #'eq)))
    (setf (au:href entry-table entry) entry)))

(defun type-table-drop (component component-type type-table)
  (remhash component (type-table component-type type-table)))

(defun eql/package-relaxed (obj1 obj2)
  (cond
    ((eql obj1 obj2) t) ; It succeeded? Oh good. Return quickly.
    ((and (symbolp obj1) (symbolp obj2)) ; Otherwise do a slower check.
     (string= (symbol-name obj1)
              (symbol-name obj2)))
    (t ; Hrm, sorry. It didn't EQL match,
     nil)))
