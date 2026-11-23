(in-package #:colony.component)

(c:define-component cuboid (comp:collider c:region-cuboid)
  ((%material :accessor material
              :initform 'x::collider/cuboid
              :annotation (c::material))
   (%obb :accessor obb
         :initform nil))
  (:default-initargs :material ))

(defmethod c:on-component-initialize ((self cuboid))
  nil)

(defmethod c:on-component-attach ((self cuboid) actor)
  (declare (ignore actor))
  (c::register-collider (c:context self) self))

(defmethod c:on-component-detach ((self cuboid) actor)
  (declare (ignore actor))
  (c::deregister-collider (c:context self) self))

(defmethod c:on-component-destroy ((self cuboid))
  (setf (referent self) nil))

;; TODO: Refactor this as it was just a quick hack
(defmethod c:on-component-physics-update ((self cuboid))
  (let* ((min (c:transform-point self
                                 (v3:+ (c:center self)
                                       (v3:vec (c:minx self)
                                               (c:miny self)
                                               (c:minz self)))))
         (max (c:transform-point self
                                 (v3:+ (c:center self)
                                       (v3:vec (c:maxx self)
                                               (c:maxy self)
                                               (c:maxz self)))))
         (center (v3:lerp min max 0.5))
         (axes (m4:rotation-to-mat3
                (m4:normalize-rotation
                 (c:get-model-matrix self))))
         (diagonal (v3:- max center))
         (half-widths (v3:vec (v3:dot diagonal (m3:get-column axes 0))
                              (v3:dot diagonal (m3:get-column axes 1))
                              (v3:dot diagonal (m3:get-column axes 2)))))
    (setf (obb self) (c::make-oriented-bounding-box center axes half-widths))))

;; TODO: When I implement the ability to not call protocol methods on types that
;; don't have them defined, ALSO create a feature that I can turn off calling
;; them for types that DO have them. Then I can leave this here and also not pay
;; the cost to render it.
(defmethod c:on-component-render ((self cuboid))
  (unless (visualize self)
    (return-from c:on-component-render))
  (u:when-let ((camera (c::active-camera (c:context self))))
    (with-material (material self)
        (:model (c:get-model-matrix self)
         :view (view camera)
         :proj (projection camera)
         :collider-local-center (c:center self)
         :in-contact-p (plusp (contact-count self))
         ;; NOTE: The shader computes the world-space math appropriately for
         ;; visualization purposes.
         :minx (c:minx self)
         :maxx (c:maxx self)
         :miny (c:miny self)
         :maxy (c:maxy self)
         :minz (c:minz self)
         :maxz (c:maxz self))

      ;; Finally, draw the visualizaiton.
      (gl:bind-vertex-array (geometry self))
      (gl:draw-arrays-instanced :points 0 1 1)
      (gl:bind-vertex-array 0))))
