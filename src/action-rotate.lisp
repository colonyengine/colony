(in-package #:virality.extensions.actions)

(defmethod action:on-update (action (type (eql 'rotate)))
  (let* ((transform (c/render::transform
                     (action:renderer (action:manager action))))
         (attrs (action:attrs action))
         (angle (or (u:href attrs :angle) (* pi 2)))
         (step (float (u:map-domain 0 1 0 angle (action:step action)) 1f0)))
    (ecase (or (u:href attrs :axis) :z)
      (:x (c/xform:rotate transform (q:orient :local :x step) :replace-p t))
      (:y (c/xform:rotate transform (q:orient :local :y step) :replace-p t))
      (:z (c/xform:rotate transform (q:orient :local :z step) :replace-p t)))))

(defmethod action:on-finish (action (type (eql 'rotate)))
  (when (action:repeat-p action)
    (action:replace action 'rotate/reverse)))

(defmethod action:on-update (action (type (eql 'rotate/reverse)))
  (let* ((transform (c/render::transform
                     (action:renderer (action:manager action))))
         (attrs (action:attrs action))
         (angle (or (u:href attrs :angle) (* pi 2)))
         (step (- angle (u:map-domain 0 1 0 angle (action:step action))))
	 (step (float step 1f0)))
    (ecase (or (u:href attrs :axis) :z)
      (:x (c/xform:rotate transform (q:orient :local :x step) :replace-p t))
      (:y (c/xform:rotate transform (q:orient :local :y step) :replace-p t))
      (:z (c/xform:rotate transform (q:orient :local :z step) :replace-p t)))))

(defmethod action:on-finish (action (type (eql 'rotate/reverse)))
  (when (action:repeat-p action)
    (action:replace action 'rotate)))
