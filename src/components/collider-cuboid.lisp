(in-package #:colony.component)

(c:define-component cuboid (c:region-cuboid)
  (;; The collider is only ever on a single layer.
   (%on-layer :accessor on-layer
              :initarg :on-layer)
   (%contact-count :accessor contact-count
                   :initform 0)
   ;; TODO: This block of slots are really here for debugging drawing of a
   ;; collider hack on it a bit to make it better.
   (%visualize :accessor visualize
               :initarg :visualize
               :initform nil)
   ;; TODO: Put geometry into storage for all sphere's to use.
   (%geometry :reader geometry
              :initform (gl:gen-vertex-array))

   (%material :reader material
              :initform 'x::collider/cuboid
              :annotation (c::material))

   ;; TODO: We do not have a difference between triggers and collisions yet.
   ;; That will come when actual physics arrives.
   ;; on-collision-enter
   ;; on-collision-continue
   ;; on-collision-exit
   ;; We'll need to add soon:
   ;; on-trigger-enter
   ;; on-trigger-continue
   ;; on-trigger-exit
   (%referent :accessor referent
              :initarg :referent
              :initform nil)
   (%obb :accessor obb
         :initform nil)))

(defmethod c:on-component-initialize ((self cuboid))
  ;; TODO
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


;; NOTE: We bubble the collision messages from the collider system through
;; ourselves to our referent (who implements this same API). This way, the
;; collider component instance can keep data about itself for visualization or
;; other purposes.
(defmethod c:on-collision-enter ((self cuboid) other-collider)
  (incf (contact-count self))
  (u:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (c:on-collision-enter referent other-collider)))

(defmethod c:on-collision-continue ((self cuboid) other-collider)
  (u:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (c:on-collision-continue referent other-collider)))

(defmethod c:on-collision-exit ((self cuboid) other-collider)
  (decf (contact-count self))
  (u:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (c:on-collision-exit referent other-collider)))
