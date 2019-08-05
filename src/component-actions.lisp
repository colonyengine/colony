(in-package #:virality.components.actions)

(v:define-component actions ()
  ((%manager :reader manager)
   (%default :reader default
             :initarg :default
             :initform nil)))

;;; Component event hooks

(defmethod v:on-component-initialize ((self actions))
  (with-slots (%manager) self
    (setf %manager (action::make-action-manager
                    (v:component-by-type (v:actor self) 'comp.render:render)
                    (default self)))))

(defmethod v:on-component-update ((self actions))
  (action::process-actions (manager self)))
