(in-package :first-light.components)

(defmethod on-collision-enter ((self (eql :a)) other-collider)
  (format t "self ~S entered collision with other collider ~S~%"
          self other-collider))

(defmethod on-collision-continue ((self (eql :a)) other-collider)
  (format t "self ~S continues collision with other collider ~S~%"
          self other-collider))

(defmethod on-collision-exit ((self (eql :a)) other-collider)
  (format t "self ~S exited collision with other collider ~S~%"
          self other-collider))






(define-component collider/sphere ()
  (;; The collider is only ever on a single layer.
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
  ;; register to communal collider db
  (let ((context (context self)))
    (declare (ignore context))
    nil))


(defmethod on-component-destroy ((self collider/sphere))
  ;; Unregister from communal collider db
  (let ((context (context self)))
    (declare (ignore context))
    nil))







;; All colliders define a COLLIDE-P method appropriate for any combination
;; that could be computed.

(defmethod collide-p ((fist collider/sphere) (face collider/sphere))
  "Return T if the two collider/spheres actually collided."
  (let* ((distance (m:distance (center fist) (center face)))
         (distance/2 (/ distance 2.0)))
    (or (<= distance/2 (radius fist))
        (<= distance/2 (radius face)))))
