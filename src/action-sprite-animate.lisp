(in-package #:virality.contrib.actions)

(defmethod v:on-action-update (action (type (eql 'sprite-animate)))
  (a:when-let* ((actor (v:actor (v:renderer (v:manager action))))
                (sprite (v:actor-component-by-type actor 'comp:sprite)))
    (comp:update-sprite-index sprite (v:action-step action))))

(defmethod v:on-action-finish (action (type (eql 'sprite-animate)))
  (when (v:repeat-p action)
    (v:replace-action action 'sprite-animate)))
