(in-package #:virality.components.collider)

(v:define-component sphere (reg:region-sphere)
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

(defmethod v:on-component-initialize ((self sphere))
  ;; TODO
  nil)

(defmethod v:on-component-attach ((self sphere) actor)
  (declare (ignore actor))
  (col::register-collider (v:context self) self))

(defmethod v:on-component-detach ((self sphere) actor)
  (declare (ignore actor))
  (col::deregister-collider (v:context self) self))

(defmethod v:on-component-destroy ((self sphere))
  (setf (referent self) nil))

;; TODO: When I implement the ability to not call protocol methods on types that
;; don't have them defined, ALSO create a feature that I can turn off calling
;; them for types that DO have them. Then I can leave this here and also not pay
;; the cost to render it.
(defmethod v:on-component-render ((self sphere))
  (unless (visualize self)
    (return-from v:on-component-render))
  (a:when-let ((camera (v::active-camera (v:context self)))
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
(defmethod v:on-collision-enter ((self sphere) other-collider)
  (incf (contact-count self))
  (a:when-let (referent (referent self))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (v:on-collision-enter referent other-collider)))

(defmethod v:on-collision-continue ((self sphere) other-collider)
  (a:when-let (referent (referent self))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (v:on-collision-continue referent other-collider)))

(defmethod v:on-collision-exit ((self sphere) other-collider)
  (decf (contact-count self))
  (a:when-let (referent (referent self))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (v:on-collision-continue referent other-collider)))

;; All colliders define a COLLIDE-P method appropriate for any combination that
;; could be computed.
;; TODO: Currently these COLLIDE-P are discrete. There needs to be a slot in the
;; collider which indicates :discrete or :continuous, and then these collider
;; functions should do the right thing if at all possible.

(defmethod collide-p ((fist sphere) (face sphere))
  "Return T if the two spheres actually collided."
  ;; A test path when testing colliders outside of FL's prefabs.
  ;; A test case, no transform component.
  (if (not (and (v:actor fist) (v:actor face)))
      (let ((distance/2 (/ (v3:distance (reg:center fist)
                                        (reg:center face)) 2f0)))
        (or (<= distance/2 (reg:radius fist))
            (<= distance/2 (reg:radius face))))
      ;; The real path through this code, which transforms the collider into
      ;; world space appropriately.
      (let* ((fist-transform (v:component-by-type (v:actor fist)
                                                  'c/xform:transform))
             (face-transform (v:component-by-type (v:actor face)
                                                  'c/xform:transform))
             ;; Figure out where the center for these colliders are in world
             ;; space.
             (fist-collider-world-center
               (c/xform:transform-point fist-transform (reg:center fist)))
             (face-collider-world-center
               (c/xform:transform-point face-transform (reg:center face)))
             ;; Figure out the size of the radius in world space. We treat the
             ;; radius as a vector and rotate/scale (but no translate!) it by the
             ;; world matrix.
             (fist-world-radius
               (c/xform:transform-vector fist-transform
                                         (v3:vec (reg:radius fist) 0f0 0f0)))
             (face-world-radius
               (c/xform:transform-vector face-transform
                                         (v3:vec (reg:radius face) 0f0 0f0)))
             ;; Compute the half way point between the two colliders.
             (distance (v3:distance fist-collider-world-center
                                    face-collider-world-center)))
        ;; Now, compute the collision is the common world space we converted
        ;; everything into.
        (<= distance (+ (v3:length fist-world-radius)
                        (v3:length face-world-radius))))))
