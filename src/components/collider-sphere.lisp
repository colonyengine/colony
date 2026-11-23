(in-package #:colony.component)

;; KEEP GOING: fix collider class symbol export to come from the right place.
(c:define-component sphere (comp:collider c:region-sphere)
  ((%material :accessor material
              :initform 'x::collider/sphere
              :annotation (c::material))))

(defmethod c:on-component-initialize ((self sphere))
  nil)

(defmethod c:on-component-attach ((self sphere) actor)
  (declare (ignore actor))
  (c::register-collider (c:context self) self))

(defmethod c:on-component-detach ((self sphere) actor)
  (declare (ignore actor))
  (c::deregister-collider (c:context self) self))

(defmethod c:on-component-destroy ((self sphere))
  (setf (referent self) nil))


;; TODO: When I implement the ability to not call protocol methods on types that
;; don't have them defined, ALSO create a feature that I can turn off calling
;; them for types that DO have them. Then I can leave this here and also not pay
;; the cost to render it.
(defmethod c:on-component-render ((self sphere))
  (unless (visualize self)
    (return-from c:on-component-render))
  (u:when-let ((camera (c::active-camera (c:context self))))
    (with-material (material self)
        (:model (c:get-model-matrix self)
         :view (view camera)
         :proj (projection camera)
         :collider-local-center (c:center self)
         :in-contact-p (plusp (contact-count self))
         ;; NOTE: The shader computes the radius appropriately for
         ;; visualization purposes.
         :radius (c:radius self))
      ;; Finally, draw the visualizaiton.
      (gl:bind-vertex-array (geometry self))
      (gl:draw-arrays-instanced :points 0 1 1)
      (gl:bind-vertex-array 0))))
