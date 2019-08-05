(in-package #:virality.components)

(v:define-component actions ()
  ((%manager :reader manager)
   (%default-actions :reader default-actions
                     :initarg :default-actions
                     :initform nil)))

;;; Component event hooks

(defmethod v:on-component-initialize ((self actions))
  (with-slots (%manager) self
    (setf %manager (v::make-action-manager
                    (v:component-by-type (v:actor self) 'render)
                    (default-actions self)))))

(defmethod v:on-component-update ((self actions))
  (v::process-actions (manager self)))
