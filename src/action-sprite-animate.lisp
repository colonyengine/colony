(in-package #:first-light.actions)

(defmethod on-action-update (action (type (eql 'sprite-animate)))
  (a:when-let* ((actor (actor (renderer (manager action))))
                (sprite (actor-component-by-type actor 'fl.comp:sprite)))
    (fl.comp:update-sprite-index sprite (action-step action))))

(defmethod on-action-finish (action (type (eql 'sprite-animate)))
  (when (repeat-p action)
    (replace-action action 'sprite-animate)))
