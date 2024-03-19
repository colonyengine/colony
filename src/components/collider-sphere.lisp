(in-package #:colony.component)

(c:define-component sphere (c:region-sphere)
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
              :initform 'x::collider/sphere
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
              :initform nil)))

(defmethod c:on-component-initialize ((self sphere))
  ;; TODO
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

;; NOTE: We bubble the collision messages from the collider system through
;; ourselves to our referent (who implements this same API). This way, the
;; collider component instance can keep data about itself for visualization or
;; other purposes.
(defmethod c:on-collision-enter ((self sphere) other-collider)
  (incf (contact-count self))
  (u:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (c:on-collision-enter referent other-collider)))

(defmethod c:on-collision-continue ((self sphere) other-collider)
  (u:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (c:on-collision-continue referent other-collider)))

(defmethod c:on-collision-exit ((self sphere) other-collider)
  (decf (contact-count self))
  (u:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (c:on-collision-exit referent other-collider)))
