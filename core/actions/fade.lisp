(in-package :first-light.actions)

(defmethod on-action-update (action (type (eql 'fade-in)))
  (let ((material (fl.comp:material (renderer (manager action)))))
    (setf (mat-uniform-ref material :opacity) (action-step action))))

(defmethod on-action-finish (action (type (eql 'fade-in)))
  (when (repeat-p action)
    (replace-action action 'fade-out)))

(defmethod on-action-update (action (type (eql 'fade-out)))
  (let ((material (fl.comp:material (renderer (manager action)))))
    (setf (mat-uniform-ref material :opacity) (- 1 (action-step action)))))

(defmethod on-action-finish (action (type (eql 'fade-out)))
  (when (repeat-p action)
    (replace-action action 'fade-in)))
