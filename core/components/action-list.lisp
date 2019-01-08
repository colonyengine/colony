(in-package :first-light.components)

(define-component actions ()
  ((manager :default nil)
   (default-actions :default nil)))

;;; Component event hooks

(defmethod on-component-initialize ((self actions))
  (with-accessors ((actor actor) (manager manager) (default-actions default-actions)) self
    (setf manager (fl.actions:make-action-manager (actor-component-by-type actor 'render)
                                                  default-actions))))

(defmethod on-component-update ((self actions))
  (fl.actions:process-actions (manager self)))
