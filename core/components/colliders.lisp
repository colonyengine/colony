(in-package :first-light.components)

(define-component collider/sphere ()
  (;; The collider is only ever on a single layer.
   (name :default "Unknown Collider") ;; Temporary, for debugging.
   (on-layer :default nil)
   (center :default (m:vec3))
   (radius :default 1.0)

   ;; TODO: We do not have a difference between triggers and collisions yet.
   ;; That will come when actual physics arrives.
   ;; on-collision-enter
   ;; on-collision-continue
   ;; on-collision-exit
   (referent :default nil)))

(defmethod on-component-initialize ((self collider/sphere))
  nil)

(defmethod on-component-attach ((self collider/sphere) actor)
  (declare (ignore actor))
  ;; register to communal collider db
  (let ((context (context self)))
    (declare (ignore context))
    nil))

(defmethod on-component-detach ((self collider/sphere) actor)
  (declare (ignore actor))
  ;; register to communal collider db
  (let ((context (context self)))
    (declare (ignore context))
    nil))

(defmethod on-component-destroy ((self collider/sphere))
  nil)

;; We'll use myself as the referent so I can debug when things happen.

(defmethod on-collision-enter ((self collider/sphere) other-collider)
  (format t "self ~S entered collision with other collider ~S~%"
          (name self) (name (referent other-collider))))

(defmethod on-collision-continue ((self collider/sphere) other-collider)
  (format t "self ~S continues collision with other collider ~S~%"
          (name self) (name (referent other-collider))))

(defmethod on-collision-exit ((self collider/sphere) other-collider)
  (format t "self ~S exited collision with other collider ~S~%"
          (name self) (name (referent other-collider))))




;; All colliders define a COLLIDE-P method appropriate for any combination
;; that could be computed.

(defmethod collide-p ((fist collider/sphere) (face collider/sphere))
  "Return T if the two collider/spheres actually collided."
  (cond
    ;; A test path when testing colliders outside of FL's prefabs.
    ((not (and (actor fist) (actor face))) ;; a test case, no transform comp.
     (let ((distance/2 (/ (m:distance (center fist) (center face)) 2.0)))
       (or (<= distance/2 (fl.comp:radius fist))
           (<= distance/2 (fl.comp:radius face)))))

    (t
     ;; The real path through this code, which transforms the collider into
     ;; world space appropriately.
     (let* ((fist-transform
              (actor-component-by-type (actor fist) 'fl.comp:transform))
            (face-transform
              (actor-component-by-type (actor face) 'fl.comp:transform))
            ;; figure out where the center for these colliders are in world
            ;; space.
            (fist-collider-world-center
              (transform-point fist-transform (center fist)))
            (face-collider-world-center
              (transform-point face-transform (center face)))
            ;; Figure out the size of the radius in world space.  We treat the
            ;; radius as a vector and rotate/scale (but no translate!) it by the
            ;; world matrix.
            (fist-world-radius
              (transform-vector fist-transform (m:vec3 (radius fist) 0 0)))
            (face-world-radius
              (transform-vector face-transform (m:vec3 (radius face) 0 0)))

            ;; Compute the half way point between the two colliders.
            (distance (m:distance fist-collider-world-center
                                  face-collider-world-center))
            (distance/2 (/ distance 2.0)))

       ;; Now, compute the collision is the common world space we converted
       ;; everything into.
       (or (<= distance/2 (m:length (m:vec3 fist-world-radius)))
           (<= distance/2 (m:length (m:vec3 face-world-radius))))))))
