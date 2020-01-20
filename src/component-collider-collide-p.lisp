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


(defun closest-pt-point-obb (world-pt obb-axes world-obb-center obb-half-widths)
  (let* ((d (v3:- world-pt world-obb-center))
         (q (v3:copy world-obb-center))
         (vec-accessor
           (make-array 3 :initial-contents (list #'v3:x #'v3:y #'v3:z))))

    (dotimes (i 3)
      (let* ((dist (v3:dot d (aref obb-axes i))))

        (when (> dist (funcall (aref vec-accessor i) obb-half-widths))
          (setf dist (funcall (aref vec-accessor i) obb-half-widths)))

        (when (< dist (- (funcall (aref vec-accessor i) obb-half-widths)))
          (setf dist (- (funcall (aref vec-accessor i) obb-half-widths))))

        (v3:+! q q (v3:scale (aref obb-axes i) dist))))
    q))

(defun %collide-p-sphere/cuboid (fist face)
  ;; NOTE!!!! fist is sphere, face is cuboid. ALWAYS.
  ;;
  ;; We're going to treat the cuboid as an OBB in world space and collide
  ;; it with the sphere also in world space.
  (let* ((fist-transform (v:component-by-type (v:actor fist)
                                              'c/xform:transform))
         (face-transform (v:component-by-type (v:actor face)
                                              'c/xform:transform))
         ;; Compute on the cuboid objects

         ;; Create an OBB from the cuboid
         (l-min (v3:+ (reg:center face)
                      (v3:vec (reg:minx face) (reg:miny face) (reg:minz face))))
         (l-max (v3:+ (reg:center face)
                      (v3:vec (reg:maxx face) (reg:maxy face) (reg:maxz face))))
         (w-min (c/xform:transform-point face-transform l-min))
         (w-max (c/xform:transform-point face-transform l-max))
         ;; center of OBB in world space.
         (w-center (v3:lerp w-min w-max .5f0))
         ;; Now get rotation axes as rotated in world-space. Page 101 of RTCD
         ;; was vague on if u in the struct OBB was a basis vector or not.
         (x-axis (v3:normalize
                  (m4:rotation-axis-to-vec3 (c/xform:model face-transform) :x)))
         (y-axis (v3:normalize
                  (m4:rotation-axis-to-vec3 (c/xform:model face-transform) :y)))
         (z-axis (v3:normalize
                  (m4:rotation-axis-to-vec3 (c/xform:model face-transform) :z)))
         (obb-axes (make-array 3 :element-type 'v3:vec
                                 :initial-contents (list x-axis y-axis z-axis)))
         (diagonal (v3:- w-max w-center))
         ;; Compute the half widths of the OBB.
         ;; TODO: Understand the effect of scaled transformation matricies.
         (half-widths (v3:vec (v3:dot diagonal x-axis)
                              (v3:dot diagonal y-axis)
                              (v3:dot diagonal z-axis)))

         ;; Get the important parts of the sphere into world space.
         (fist-collider-world-center
           (c/xform:transform-point fist-transform (reg:center fist)))
         (fist-world-radius
           (v3:length
            (c/xform:transform-vector fist-transform
                                      (v3:vec (reg:radius fist) 0f0 0f0)))))

    (let* ((p (closest-pt-point-obb fist-collider-world-center
                                    obb-axes w-center half-widths))
           (v (v3:- p fist-collider-world-center)))

      (<= (v3:dot v v) (expt fist-world-radius 2f0)))))


(defmethod collide-p ((fist sphere) (face cuboid))
  (%collide-p-sphere/cuboid fist face))


(defmethod collide-p ((fist cuboid) (face sphere))
  (%collide-p-sphere/cuboid face fist))

(defmethod collide-p ((fist cuboid) (face cuboid))
  (error "XXX: Implement collide-p CUBOID/CUBOID~%")
  nil)
