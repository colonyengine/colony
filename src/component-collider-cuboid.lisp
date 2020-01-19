(in-package #:virality.components.collider)

(v:define-component cuboid (reg:region-cuboid)
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
   ;; TODO: Put geometry into shared storage for all sphere's to use.
   (%geometry :reader geometry
              :initform (gl:gen-vertex-array))

   ;; TODO: need one to render a cuboid.
   (%material :reader material
              :initform 'x/mat::collider/sphere
              :annotation (v::material))

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

(defmethod v:on-component-initialize ((self cuboid))
  ;; TODO
  nil)

(defmethod v:on-component-attach ((self cuboid) actor)
  (declare (ignore actor))
  (col::register-collider (v:context self) self))

(defmethod v:on-component-detach ((self cuboid) actor)
  (declare (ignore actor))
  (col::deregister-collider (v:context self) self))

(defmethod v:on-component-destroy ((self cuboid))
  (setf (referent self) nil))


;; TODO: When I implement the ability to not call protocol methods on types that
;; don't have them defined, ALSO create a feature that I can turn off calling
;; them for types that DO have them. Then I can leave this here and also not pay
;; the cost to render it.
(defmethod v:on-component-render ((self cuboid))
  ;; disable this.
  (return-from v:on-component-render nil)

  ;; FIXME when a new shader/material is created to draw a cuboid collider.
  #++(unless (visualize self)
       (return-from v:on-component-render))
  #++(a:when-let ((camera (v::active-camera (v:context self)))
                  (transform (v:component-by-type (v:actor self)
                                                  'c/xform:transform)))
       (mat:with-material (material self)
           (:model (c/xform:model transform)
            :view (c/cam:view camera)
            :proj (c/cam:projection camera)
            :collider-local-position (reg:center self)
            :in-contact-p (plusp (contact-count self))
            ;; NOTE: The shader computes the radius appropriately for
            ;; visualization purposes.
            :radius (reg:radius self))


         ;; Finally, draw the visualizaiton.
         (gl:bind-vertex-array (geometry self))
         (gl:draw-arrays-instanced :points 0 1 1)
         (gl:bind-vertex-array 0))))


;; NOTE: We bubble the collision messages from the collider system through
;; ourselves to our referent (who implements this same API). This way, the
;; collider component instance can keep data about itself for visualization or
;; other purposes.
(defmethod v:on-collision-enter ((self cuboid) other-collider)
  (incf (contact-count self))
  (a:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (v:on-collision-enter referent other-collider)))

(defmethod v:on-collision-continue ((self cuboid) other-collider)
  (a:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (v:on-collision-continue referent other-collider)))

(defmethod v:on-collision-exit ((self cuboid) other-collider)
  (decf (contact-count self))
  (a:when-let ((referent (referent self)))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (v:on-collision-continue referent other-collider)))
