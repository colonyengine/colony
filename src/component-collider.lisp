(in-package #:first-light.components)

(define-component collider/sphere ()
  (;; The collider is only ever on a single layer.
   (%on-layer :accessor on-layer
              :initarg :on-layer)
   (%center :accessor center
            :initarg :center
            :initform (v3:zero))
   (%radius :accessor radius
            :initarg :radius
            :initform 1.0)
   (%num-contacts :accessor num-contacts
                  :initform 0)
   ;; TODO: This block of slots are really here for debugging drawing of a
   ;; collider hack on it a bit to make it better.
   (%visualize :accessor visualize
               :initarg :visualize
               :initform nil)
   ;; TODO: Put geometry into shared storage for all collider/sphere's to use.
   (%geometry :reader geometry
              :initform (gl:gen-vertex-array))
   (%material :reader material
              :initform 'fl.materials::collider/sphere
              :annotation (fl.annotations:material))
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

(defmethod on-component-initialize ((self collider/sphere))
  ;; TODO
  nil)

(defmethod on-component-attach ((self collider/sphere) actor)
  (declare (ignore actor))
  (let ((context (context self)))
    (register-collider context self)))

(defmethod on-component-detach ((self collider/sphere) actor)
  (declare (ignore actor))
  (let ((context (context self)))
    (deregister-collider context self)))

(defmethod on-component-destroy ((self collider/sphere))
  (setf (referent self) nil))

;; TODO: When I implement the ability to not call protocol methods on types that
;; don't have them defined, ALSO create a feature that I can turn off calling
;; them for types that DO have them. Then I can leave this here and also not pay
;; the cost to render it.
(defmethod on-component-render ((self collider/sphere))
  (unless (visualize self)
    (return-from on-component-render))
  (a:when-let ((camera (active-camera (context self))))
    (let ((transform (actor-component-by-type (actor self) 'fl.comp:transform)))
      (using-material (material self)
          (:model (fl.comp:model transform)
           :view (fl.comp:view camera)
           :proj (fl.comp:projection camera)
           :collider-local-position (center self)
           :in-contact-p (> (num-contacts self) 0)
           ;; NOTE: The shader computes the radius appropriately for
           ;; visualization purposes.
           :radius (radius self))
        ;; Finally, draw the visualizaiton.
        (gl:bind-vertex-array (geometry self))
        (gl:draw-arrays-instanced :points 0 1 1)
        (gl:bind-vertex-array 0)))))

;; NOTE: We bubble the collision messages from the collider system through
;; ourselves to our referent (who implements this same API). This way, the
;; collider component instance can keep data about itself for visualization or
;; other purposes.
(defmethod on-collision-enter ((self collider/sphere) other-collider)
  (incf (num-contacts self))
  (a:when-let (referent (referent self))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (on-collision-enter referent other-collider)))

(defmethod on-collision-continue ((self collider/sphere) other-collider)
  (a:when-let (referent (referent self))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (on-collision-continue referent other-collider)))

(defmethod on-collision-exit ((self collider/sphere) other-collider)
  (decf (num-contacts self))
  (a:when-let (referent (referent self))
    (when (eq self referent)
      (error "The referent of a collider must not be same collider component!"))
    (on-collision-continue referent other-collider)))

;; All colliders define a COLLIDE-P method appropriate for any combination that
;; could be computed.
;; TODO: Currently these COLLIDE-P are discrete. There needs to be a slot in the
;; collider which indicates :discrete or :continuous, and then these collider
;; functions should do the right thing if at all possible.

(defmethod collide-p ((fist collider/sphere) (face collider/sphere))
  "Return T if the two collider/spheres actually collided."
  (cond
    ;; A test path when testing colliders outside of FL's prefabs.
    ((not (and (actor fist) (actor face))) ;; a test case, no transform comp.
     (let ((distance/2 (/ (v3:distance (center fist) (center face)) 2.0)))
       (or (<= distance/2 (fl.comp:radius fist))
           (<= distance/2 (fl.comp:radius face)))))
    (t
     ;; The real path through this code, which transforms the collider into
     ;; world space appropriately.
     (let* ((fist-transform
              (actor-component-by-type (actor fist) 'fl.comp:transform))
            (face-transform
              (actor-component-by-type (actor face) 'fl.comp:transform))
            ;; Figure out where the center for these colliders are in world
            ;; space.
            (fist-collider-world-center
              (transform-point fist-transform (center fist)))
            (face-collider-world-center
              (transform-point face-transform (center face)))
            ;; Figure out the size of the radius in world space. We treat the
            ;; radius as a vector and rotate/scale (but no translate!) it by the
            ;; world matrix.
            (fist-world-radius
              (transform-vector fist-transform (v3:vec (radius fist) 0 0)))
            (face-world-radius
              (transform-vector face-transform (v3:vec (radius face) 0 0)))
            ;; Compute the half way point between the two colliders.
            (distance (v3:distance fist-collider-world-center
                                   face-collider-world-center)))
       ;; Now, compute the collision is the common world space we converted
       ;; everything into.
       (<= distance (+ (v3:length fist-world-radius)
                       (v3:length face-world-radius)))))))
