(in-package #:virality.contrib.actions)

(defmethod action:on-update (action (type (eql 'fade-in)))
  (let ((material (comp.render:material
                   (action:renderer (action:manager action)))))
    (setf (v:uniform-ref material :opacity) (action:step action))))

(defmethod action:on-finish (action (type (eql 'fade-in)))
  (when (action:repeat-p action)
    (action:replace action 'fade-out)))

(defmethod action:on-update (action (type (eql 'fade-out)))
  (let ((material (comp.render:material
                   (action:renderer (action:manager action)))))
    (setf (v:uniform-ref material :opacity) (- 1 (action:step action)))))

(defmethod action:on-finish (action (type (eql 'fade-out)))
  (when (action:repeat-p action)
    (action:replace action 'fade-in)))
