(in-package #:virality.components.collider)

;; All colliders define a COLLIDE-P method appropriate for any combination that
;; could be computed.
;; TODO: Currently these COLLIDE-P are discrete. There needs to be a slot in the
;; collider which indicates :discrete or :continuous, and then these collider
;; functions should do the right thing if at all possible.

(defmethod collide-p ((fist sphere) (face sphere))
  "Return T if the two spheres actually collided."
  (if (not (and (v:actor fist) (v:actor face)))
      ;; NOTE: This code path is here for testing the collider system when
      ;; not running V properly. It should be removed when we figure out how
      ;; to test V's collision system better.
      (let ((distance/2 (/ (v3:distance (reg:center fist)
                                        (reg:center face)) 2f0)))
        (or (<= distance/2 (reg:radius fist))
            (<= distance/2 (reg:radius face))))

      ;; This is the REAL path through this code, which transforms the collider
      ;; into world space appropriately.
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
             ;; radius as a vector and rotate/scale (but no translate!) it by
             ;; the world matrix.
             (fist-world-radius
               (c/xform:transform-vector fist-transform
                                         (v3:vec (reg:radius fist) 0f0 0f0)))
             (face-world-radius
               (c/xform:transform-vector face-transform
                                         (v3:vec (reg:radius face) 0f0 0f0)))

	     ;; TODO: Allow the gamedev an ability to ensure that the world
	     ;; matrix must be uniform for a sphere collider.  This involves
	     ;; calculating the magnitude of each of the rotation axes and
	     ;; checking they are within a small tolerance of each other.  Don't
	     ;; check it here, but it should be checked somewhere.

             ;; Compute the half way point between the two colliders.
             (distance (v3:distance fist-collider-world-center
                                    face-collider-world-center)))
        ;; Now, compute the collision is the common world space we converted
        ;; everything into.
        (<= distance (+ (v3:length fist-world-radius)
                        (v3:length face-world-radius))))))

(defmethod collide-p ((fist sphere) (face cuboid))
  nil)

(defmethod collide-p ((fist cuboid) (face cuboid))
  nil)

(defmethod collide-p ((fist cuboid) (face sphere))
  nil)
