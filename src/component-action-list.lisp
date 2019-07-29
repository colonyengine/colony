(in-package #:first-light.components)

(define-component actions ()
  ((%manager :reader manager)
   (%default-actions :reader default-actions
                     :initarg :default-actions
                     :initform nil)))

;;; Component event hooks

(defmethod on-component-initialize ((self actions))
  (with-slots (%manager) self
    (setf %manager (fl.actions:make-action-manager
                    (actor-component-by-type (actor self) 'render)
                    (default-actions self)))))

(defmethod on-component-update ((self actions))
  (fl.actions:process-actions (manager self)))
